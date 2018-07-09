;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; framework.lisp

(in-package :raptor-launcher/raptor-picker)
(in-readtable :qtools)

(defclass sync-step ()
  ((%name :accessor name
          :initarg :name
          :initform (error "Must provide NAME."))
   ;; Must be a symbol
   (%download-fn :accessor download-fn
                 :initarg :download-fn
                 :initform #'identity)
   ;; Returns downloaded data that must be accepted by MERGE-FN and SPAWN-FN
   (%merge-fn :accessor merge-fn
              :initarg :merge-fn
              :initform #'identity)
   ;; Returns if any data was merged
   (%spawn-fn :accessor spawn-fn
              :initarg :spawn-fn
              :initform #'identity)
   ;; Returns the amount of spawned threads
   (%done-fn :accessor done-fn
             :initarg :done-fn
             :initform (constantly nil))
   ;; Return value is ignored
   ))

(defun make-sync-step (name download-fn merge-fn spawn-fn done-fn)
  (make-instance 'sync-step :name name
                            :download-fn download-fn :merge-fn merge-fn
                            :spawn-fn spawn-fn :done-fn done-fn))

(defvar *step-format* "~@[~A~] ~A~@[ for ~A]: ~A.")

(defvar *step-type* "Step")

(defgeneric call-sync-step (step &rest initial-data)
  (:documentation "Executes the synchronization step and all of its functions.
\
The first element of INITIAL-DATA will be pretty-printed in the logs, unless it
is set to NIL.")
  (:method (step &rest initial-data)
    (let ((name (name step)))
      (flet ((debug (message)
               (note t :debug *step-format* *step-type*
                     name (first initial-data) message)))
        (debug "begin")
        (let ((data (apply (download-fn step) initial-data)))
          (debug "downloaded")
          (when (apply (merge-fn step) data)
            (debug "merged"))
          (loop with format-spawn = "~D tasks spawned for step ~A"
                with steps = (next-steps step)
                for step in steps
                for n = (apply (spawn-fn step) step data)
                unless (= 0 n)
                  do (debug (format nil format-spawn n step)))
          (apply (done-fn step) data)
          (debug "done")))))
  (:method :around (step &rest initial-data)
    (let ((name (name step)))
      (handler-case (progn (call-next-method) t)
        (error (e)
          (note t :error *step-format* *step-type*
                name (first initial-data) e)
          nil)))))

(defvar *flow-step-next-steps* (make-hash-table))

(defvar *flow-steps* (make-hash-table))

(defvar *current-flow* nil)

;; TODO
(define-flow synchronize (picker accounts)
  (login            -> download-account)
  (download-account -> download-furres
                    -> download-images)
  (download-furres  -> download-costumes
                    -> download-portraits
                    -> download-specitags))

(defun run-flow (flow-name &rest initial-data)
  (let* ((*current-flow* flow-name)
         (*step-type* "Synchronization step")
         (step (gethash *flow-steps* flow-name)))
    ))

(define-step (synchronize login) (email password picker)
  (make-sync-step
   'login
   (lambda (email password picker)
     (let ((cookie-jar (cl-furcadia/ws:login email password)))
       (unless cookie-jar (error "Invalid credentials for email ~A." email))
       (list email cookie-jar picker)))
   (lambda (email cookie-jar picker)
     (with-lock-held ((lock picker))
       (setf (gethash email (emails-cookie-jars picker)) cookie-jar)
       t))
   (lambda (step email cookie-jar picker)
     (loop with queue = (queue picker)
           with steps = (next-steps step)
           for step in steps
           for fn = (curry #'call-sync-step step email cookie-jar picker)
           for name = (format nil "RL account thread: ~A" email)
           for thread = (make-thread fn :name name)
           do (push-queue thread queue)
           finally (return (length steps))))
   (lambda (email cookie-jar picker)
     (signal! picker (login-completed string) email))))
