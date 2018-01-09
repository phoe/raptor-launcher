;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; backend.lisp

(in-package :raptor-launcher/raptor-picker)
(in-readtable :qtools)

(define-slot (raptor-picker sync) ()
  (declare (connected sync-button (clicked)))
  (sync raptor-picker))

;;; Sync started

(define-signal (raptor-picker sync-started) ())

(define-slot (raptor-picker got-sync-started) ()
  (declare (connected raptor-picker (sync-started)))
  ;; (setf (q+:text (loading-screen raptor-picker))
  ;;       "Synchronization in progress.")
  (q+:hide furre-list)
  (q+:hide image)
  (q+:show (loading-screen raptor-picker)))

;;; Login completed

(define-signal (raptor-picker login-completed) (string)) ;; email

(define-slot (raptor-picker got-login-completed) ((email string))
  (declare (connected raptor-picker (login-completed string))))

;;; Account downloaded

(define-signal (raptor-picker account-downloaded)
               (string int)) ;; email nfurres

;;; Furre downloaded

(define-signal (raptor-picker furre-downloaded)
               (string int int int)) ;; sname nspecitags nportraits ncostumes

;;; Specitag downloaded

(define-signal (raptor-picker specitag-downloaded) (string int)) ;; sname id

;;; Portrait downloaded

(define-signal (raptor-picker portrait-downloaded) (string int)) ;; sname id

;;; Costume downloaded

(define-signal (raptor-picker costume-downloaded) (string int)) ;; sname id

;;; Image downloaded

(define-signal (raptor-picker image-downloaded) (string)) ;; sname

;;; Synchronization complete

(define-signal (raptor-picker sync-complete) (bool)) ;; successp errormsg

(define-slot (raptor-picker got-sync-complete) ((successp bool))
  (declare (connected raptor-picker (sync-complete bool)))
  (case successp
    ((nil)
     ;; (setf (q+:text (loading-screen raptor-picker)) TODO
     ;;       "Synchronization failed - see logs for details.")
     )
    ((t)
     (q+:show furre-list)
     (q+:show image)
     (q+:hide (loading-screen raptor-picker)))))

;;; Sync

(defmethod sync ((picker raptor-picker))
  (note t :info "Synchronization started.")
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
          (make-thread (curry #'join-all-queued picker) :name "RL joiner"))))

;;; TODO specify somewhere that all workers must return non-NIL if they were
;;; successful. Somewhere better than this comment, that is.

;;; Login

(defun worker-login (picker email password)
  (note t :debug "Attempting to log in as ~A." email)
  (handler-case
      (let ((cookie-jar (cl-furcadia/ws:login email password)))
        (unless cookie-jar
          (error "Invalid credentials for email ~A." email))
        (with-lock-held ((lock picker))
          (setf (gethash email (emails-cookie-jars picker)) cookie-jar))
        (note t :info "Login as ~A succeeded." email)
        (let* ((queue (queue picker))
               (fn (curry #'worker-account picker cookie-jar email))
               (name (format nil "RL account thread: ~A" email))
               (thread (make-thread fn :name name)))
          (push-queue thread queue))
        (signal! picker (login-completed string) email)
        t)
    (error (e)
      (note t :error "Login as ~A failed: ~A" email e)
      (signal e))))

;;; Download account

(defun worker-account (picker cookie-jar email)
  (note t :debug "Fetching account ~A." email)
  (handler-case
      (multiple-value-bind (account snames last-logins)
          (cl-furcadia/ws:fetch-account cookie-jar)
        (with-lock-held ((lock picker))
          (setf (gethash email (emails-accounts picker)) account))
        (note t :info "Fetching account ~A succeeded." email)
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
        for fn = (curry #'worker-furre picker sname last-login account cookie-jar)
        for name = (format nil "RL furre fetcher: ~A" sname)
        for thread = (make-thread fn :name name)
        do (push-queue thread queue)))

;;; Download furres

;;; TODO split these into downloading (handler-bound) and spawning new threads
;;; (not handler-bound)
(defun worker-furre (picker sname last-login account cookie-jar)
  (note t :debug "Fetching furre ~A." sname)
  (handler-case
      (multiple-value-bind (furre unknowns)
          (cl-furcadia/ws:fetch-furre sname cookie-jar)
        (when unknowns
          (note t :warn " Unknown keywords in furre response (bug?): ~A"
                unknowns))
        (setf (cl-furcadia:last-login furre) last-login)
        (with-lock-held ((lock picker))
          (push furre (cl-furcadia:furres account)))
        (note t :info "Fetching furre ~A succeeded." sname)
        (%worker-furre-spawn-detail-threads picker furre cookie-jar)
        (let ((nspecitags (length (cl-furcadia:specitags furre)))
              (nportraits (length (cl-furcadia:portraits furre)))
              (ncostumes (length (cl-furcadia:costumes furre))))
          (signal! picker (furre-downloaded string int int int)
                   sname nspecitags nportraits ncostumes)
          t))
    (error (e)
      (note t :error "Failed to fetch character ~A: ~A" sname e)
      (error e))))

(defun %worker-furre-spawn-detail-threads (picker furre cookie-jar)
  (declare (ignore picker furre cookie-jar))
  )

;;; Join all threads and complete synchronization

(defun join-all-queued (picker)
  (let ((errors 0)
        (queue (queue picker)))
    (loop for thread = (try-pop-queue queue)
          while thread
          do (note t :trace "Waiting for task ~S to complete."
                   (thread-name thread))
             (handler-case
                 (cond ((join-thread thread)
                        (note t :trace "Task ~S completed successfully."
                              (thread-name thread)))
                       (t (incf errors)
                          (note t :trace "Task ~S completed unsuccessfully."
                                (thread-name thread))))
               (error (e)
                 (incf errors)
                 (note t :warn "Task ~S completed with an error: ~A."
                       (thread-name thread) e))))
    (case errors
      (0
       (note t :info "Synchronization complete.")
       (signal! picker (sync-complete bool) t))
      (t
       (note t :warn "Synchronization complete, but errors have occurred.")
       (signal! picker (sync-complete bool) nil)))))

;;; Fetch accounts from available config widgets

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
