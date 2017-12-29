;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; backend.lisp

(in-package :raptor-launcher/raptor-picker)
(in-readtable :qtools)

(define-slot (raptor-picker sync) ()
  (declare (connected sync-button (clicked)))
  (sync raptor-picker))

(define-signal (raptor-picker sync-started) ())

(define-signal (raptor-picker login-completed) (string)) ;; email

(define-signal (raptor-picker account-downloaded)
               (string int)) ;; email nfurres

(define-signal (raptor-picker furre-downloaded)
               (string int int int)) ;; sname nspecitags nportraits ncostumes

(define-signal (raptor-picker specitag-downloaded) (string int)) ;; sname id

(define-signal (raptor-picker portrait-downloaded) (string int)) ;; sname id

(define-signal (raptor-picker costume-downloaded) (string int)) ;; sname id

(define-signal (raptor-picker image-downloaded) (string)) ;; sname

(define-signal (raptor-picker sync-complete) (bool string)) ;; successp errormsg

(define-slot (raptor-picker got-login-completed) ((email string))
  (declare (connected raptor-picker (login-completed string)))
  ;; (format t "Login completed! ~A~%" email)
  )

;;; Sync

(defmethod sync ((picker raptor-picker))
  (signal! picker (sync-started))
  (let* ((modules (loaded-modules *main-window*))
         (configs (remove-if-not (rcurry #'subclassp 'config) modules
                                 :key #'class-of))
         (accounts (configs-accounts configs))
         (queue (queue picker)))
    (loop for (email password) in accounts
          for fn = (curry #'worker-login picker email password)
          for name = (format nil "RL login thread: ~A" email)
          for thread = (make-thread fn :name name)
          do (push-queue thread queue))
    (setf (queue-joiner picker)
          (make-thread (curry #'join-all-queued queue) :name "RL joiner"))))

;;; TODO specify somewhere that all workers must return non-NIL if they were
;;; successful. Somewhere better than this comment, that is.

;;; Login

(defun worker-login (picker email password)
  (note t :debug "Attempting to log in as ~A." email)
  (handler-case
      (let ((cookie-jar (cl-furcadia/ws:login email password)))
        (unless cookie-jar
          (error "Invalid credentials for email ~A." email))
        (note t :info "Login as ~A succeeded." email)
        (with-lock-held ((lock picker))
          (setf (gethash email (emails-cookie-jars picker)) cookie-jar))
        (let* ((queue (queue picker))
               (fn (curry #'worker-account picker cookie-jar email))
               (name (format nil "RL account thread: ~A" email))
               (thread (make-thread fn :name name)))
          (push-queue thread queue))
        (signal! picker (login-completed string) email)
        t)
    (error (e)
      (note t :error "Login as ~A failed: ~A." email e)
      (signal e))))

;;; Download account

(defun worker-account (picker cookie-jar email)
  (note t :debug "Fetching account ~A." email)
  (handler-case
      (multiple-value-bind (account snames last-logins)
          (cl-furcadia/ws:fetch-account cookie-jar)
        (note t :info "Fetching account ~A succeeded." email)
        (with-lock-held ((lock picker))
          (setf (gethash email (emails-accounts picker)) account))
        (%worker-account-spawn-furre-threads picker account snames
                                             last-logins cookie-jar)
        (signal! picker (account-downloaded string int) email (length snames))
        t)
    (error (e)
      (note t :error "Failed to fetch account ~A: ~A" email e)
      (error e))))

(defun %worker-account-spawn-furre-threads
    (picker account snames last-logins cookie-jar)
  (loop with queue = (queue picker)
        for sname in snames
        for last-login in last-logins
        for fn = (curry #'worker-furre sname last-login account cookie-jar)
        for name = (format nil "RL furre fetcher: ~A" sname)
        for thread = (make-thread fn :name name)
        do (push-queue thread queue)))

;;; Download furres

(defun worker-furre (sname last-login account cookie-jar)
  (declare (ignore sname last-login account cookie-jar))
  ;; #'cl-furcadia/ws:fetch-furre
  t
  )

(defun join-all-queued (queue)
  (let ((errors 0))
    (loop for thread = (try-pop-queue queue)
          while thread
          do (note t :trace "Waiting for task ~S to complete."
                   (thread-name thread))
             (cond ((join-thread thread)
                    (note t :trace "Task ~S completed successfully."
                          (thread-name thread)))
                   (t (incf errors)
                      (note t :warn "Task ~S completed with an error."
                            (thread-name thread)))))
    (if (= 0 errors)
        (note t :debug "All sync operations have completed.")
        (note t :warn "All sync operations have completed with errors."))))

(defvar *no-configs*
  "No configuration modules found to fetch account information from.")

(defvar *multiple-configs*
  "Multiple configuration modules found: ~{~A~^, }. Using the first one.")

(defun configs-accounts (configs)
  (case (length configs)
    (0 (note t :severe *no-configs*) '())
    (1 (accounts (first configs)))
    (t (note t :warn *multiple-configs* (mapcar #'class-of configs))
     (accounts (first configs)))))
