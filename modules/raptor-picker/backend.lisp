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

;;; Sync

(defmethod sync ((picker raptor-picker))
  (signal! picker (sync-started)))

(define-signal (raptor-picker sync-started) ())

(define-slot (raptor-picker got-sync-started) ()
  (declare (connected raptor-picker (sync-started)))
  (note t :info "Synchronization started.")
  (let ((loading-screen (loading-screen raptor-picker)))
    (q+:hide furre-list)
    (q+:hide image)
    (q+:show loading-screen)
    (reset loading-screen)
    (let* ((naccounts (length (hash-table-keys (config :config :accounts)))))
      (setf (maximum loading-screen 'progress-logins) naccounts
            (maximum loading-screen 'progress-accounts) naccounts)))
  (let* ((petri-net (make-picker-petri-net))
         (accounts (get-accounts)))
    (dolist (account accounts)
      (bag-insert (bag-of petri-net 'credentials) account))
    (setf (petri-net-of raptor-picker) petri-net)
    (make-thread (curry #'execute-sync raptor-picker)
                 :name "Raptor Picker Petri net thread")))

(defun execute-sync (picker)
  (let* ((petri-net (petri-net-of picker))
         (errorp (nth-value 1 (funcall petri-net :ignore-errors t))))
    (if errorp
        (note t :warn "Synchronization complete, but errors have occurred.")
        (note t :info "Synchronization complete."))
    (apply #'note t :info "Downloaded ~D accounts, ~D furres, ~D costumes, ~D ~
portraits, ~D specitags, and ~D images."
           (mapcar #'bag-count
                   (mapcar (curry #'bag-of petri-net)
                           '(accounts furres costumes
                             portraits specitags images))))
    (signal! picker (sync-complete bool) (not errorp))))

;;; Login

(define-signal (raptor-picker login-completed) (string)) ;; email

(define-slot (raptor-picker got-login-completed) ((email string))
  (declare (connected raptor-picker (login-completed string)))
  (incf (current (loading-screen raptor-picker) 'progress-logins)))

;;; Account downloaded

(define-signal (raptor-picker account-downloaded)
               (string int)) ;; email nfurres

(define-slot (raptor-picker got-account-downloaded)
             ((email string) (nfurres int))
  (declare (connected raptor-picker (account-downloaded string int)))
  (incf (current (loading-screen raptor-picker) 'progress-accounts))
  (incf (maximum (loading-screen raptor-picker) 'progress-furres) nfurres))

;;; Furre downloaded

(define-signal (raptor-picker furre-downloaded)
               (string int int int)) ;; sname nspecitags nportraits ncostumes

(define-slot (raptor-picker got-furre-downloaded)
             ((sname string) (nspecitags int) (nportraits int) (ncostumes int))
  (declare (connected raptor-picker (furre-downloaded string int int int)))
  (incf (current (loading-screen raptor-picker) 'progress-furres))
  (incf (current (loading-screen raptor-picker) 'progress-costumes))
  (incf (maximum (loading-screen raptor-picker) 'progress-portraits) nportraits)
  (incf (maximum (loading-screen raptor-picker) 'progress-costumes) ncostumes))

;;; Costume downloaded

(define-signal (raptor-picker costume-downloaded) (string int)) ;; sname id

(define-slot (raptor-picker got-costume-downloaded)
             ((sname string) (ncostumes int))
  (declare (connected raptor-picker (costume-downloaded string int)))
  (incf (current (loading-screen raptor-picker) 'progress-costumes)))

;;; Specitag downloaded

(define-signal (raptor-picker specitag-downloaded) (string int)) ;; sname id

(define-slot (raptor-picker got-specitag-downloaded)
             ((sname string) (nspecitags int))
  (declare (connected raptor-picker (specitag-downloaded string int)))
  (incf (current (loading-screen raptor-picker) 'progress-specitags)))

;;; Portrait downloaded

(define-signal (raptor-picker portrait-downloaded) (string int)) ;; sname id

(define-slot (raptor-picker got-portrait-downloaded)
             ((sname string) (nportraits int))
  (declare (connected raptor-picker (portrait-downloaded string int)))
  (incf (current (loading-screen raptor-picker) 'progress-portraits)))

;;; Image list downloaded

(define-signal (raptor-picker image-list-downloaded)
               (string int)) ;; sname nimages

(define-slot (raptor-picker got-image-list-downloaded)
             ((sname string) (nimages int))
  (declare (connected raptor-picker (image-list-downloaded string int)))
  (incf (maximum (loading-screen raptor-picker) 'progress-images) nimages))

;;; Image downloaded

(define-signal (raptor-picker image-downloaded) (string int)) ;; sname id

(define-slot (raptor-picker got-image-downloaded)
             ((sname string) (nimages int))
  (declare (connected raptor-picker (image-downloaded string int)))
  (incf (current (loading-screen raptor-picker) 'progress-images)))

;;; Join all threads and complete synchronization

(define-signal (raptor-picker sync-complete) (bool)) ;; successp

(define-slot (raptor-picker got-sync-complete) ((successp bool))
  (declare (connected raptor-picker (sync-complete bool)))
  (case successp
    ((nil)
     ;; TODO
     )
    ((t)
     (q+:hide (loading-screen raptor-picker))
     (q+:show furre-list)
     (q+:show image))))
