;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; backend.lisp

(in-package :raptor-launcher/raptor-picker)
(in-readtable :qtools)

;;; Fetch accounts from available config widgets
;;; TODO move to utils of some sort
;;; TODO move to config protocol
;;; TODO generify?, there is more than #'accounts to be called

(defvar *no-configs*
  "No configuration modules found to fetch account information from.")

(defvar *multiple-configs*
  "Multiple configuration modules found: ~{~A~^, }. Using the first one.")

(defun get-accounts (&optional (main-window *main-window*))
  (let* ((modules (loaded-modules main-window))
         (configs (remove-if-not (rcurry #'subclassp 'config) modules
                                 :key #'class-of)))
    (case (length configs)
      (0 (note t :severe *no-configs*) nil)
      (1 (accounts (first configs)))
      (t (note t :warn *multiple-configs* (mapcar #'class-of configs))
       (accounts (first configs))))))

(defmethod sync ((picker raptor-picker))
  (signal! picker (sync-started)))

(defmacro define-step
    ((object signal-name) (&rest typed-bindings) &body body)
  (let ((types (mapcar #'second typed-bindings)))
    `(progn
       (define-signal (,object ,signal-name) ,types)
       (define-slot (,object ,(symbolicate "GOT-" signal-name)) ,typed-bindings
         (declare (connected ,object (,signal-name ,@types)))
         ,@body))))

;;; Steps

(define-step (raptor-picker sync-started) ()
  (note t :info "Synchronization started.")
  (let ((loading-screen (loading-screen raptor-picker)))
    (q+:hide furre-list)
    (q+:hide image)
    (q+:show loading-screen)
    (reset loading-screen)
    (let* ((naccounts (hash-table-count (config :config :credentials))))
      (setf (maximum loading-screen 'progress-logins) naccounts
            (maximum loading-screen 'progress-accounts) naccounts)))
  (let* ((petri-net (make-picker-petri-net))
         (accounts (get-accounts)))
    (dolist (account accounts)
      (bag-insert (bag-of petri-net 'credentials) account))
    (setf (petri-net-of raptor-picker) petri-net)
    (make-thread
     (named-lambda execute-sync ()
       (let* ((petri-net (petri-net-of raptor-picker))
              (errorp (nth-value 1 (funcall petri-net :ignore-errors t))))
         (signal! raptor-picker (sync-complete bool) (not errorp))))
     :name "Raptor Picker Petri net thread")))

(define-step (raptor-picker login-completed) ((email string))
  (incf (current (loading-screen raptor-picker) 'progress-logins)))

(define-step (raptor-picker account-downloaded)
    ((email string) (nfurres int))
  (incf (current (loading-screen raptor-picker) 'progress-accounts))
  (incf (maximum (loading-screen raptor-picker) 'progress-furres) nfurres))

(define-step (raptor-picker furre-downloaded)
    ((sname string) (nspecitags int) (nportraits int) (ncostumes int))
  (incf (current (loading-screen raptor-picker) 'progress-furres))
  (incf (current (loading-screen raptor-picker) 'progress-costumes))
  (incf (maximum (loading-screen raptor-picker) 'progress-portraits) nportraits)
  (incf (maximum (loading-screen raptor-picker) 'progress-costumes) ncostumes))

(define-step (raptor-picker costume-downloaded)
    ((sname string) (ncostumes int))
  (incf (current (loading-screen raptor-picker) 'progress-costumes)))

(define-step (raptor-picker specitag-downloaded)
    ((sname string) (nspecitags int))
  (incf (current (loading-screen raptor-picker) 'progress-specitags)))

(define-step (raptor-picker portrait-downloaded)
    ((sname string) (nportraits int))
  (incf (current (loading-screen raptor-picker) 'progress-portraits)))

(define-step (raptor-picker image-list-downloaded)
    ((sname string) (nimages int))
  (incf (maximum (loading-screen raptor-picker) 'progress-images) nimages))

(define-step (raptor-picker image-downloaded)
    ((sname string) (nimages int))
  (incf (current (loading-screen raptor-picker) 'progress-images)))

(define-step (raptor-picker sync-complete) ((successp bool))
  (let ((petri-net (petri-net-of raptor-picker)))
    (if successp
        (note t :info "Synchronization complete.")
        (note t :warn "Synchronization complete, but errors have occurred."))
    (apply #'note t :info "Downloaded ~D accounts, ~D furres, ~D costumes, ~D ~
portraits, ~D specitags, and ~D images."
           (mapcar #'bag-count
                   (mapcar (curry #'bag-of petri-net)
                           '(accounts furres costumes
                             portraits specitags images)))))
  (q+:hide (loading-screen raptor-picker))
  (q+:show furre-list)
  (q+:show image))
