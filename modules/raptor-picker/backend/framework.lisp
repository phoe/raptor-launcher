;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; framework.lisp

(in-package :raptor-launcher/raptor-picker)
(in-readtable :qtools)

(defclass step ()
  ((%name :accessor name
          :initarg :name
          :initform (error "Must provide NAME."))
   ;; Must be a symbol
   (%fetch :accessor fetch
           :initarg :fetch
           :initform #'identity)
   ;; Returns downloaded data that must be accepted by MERGE and SPAWN
   (%merge :accessor merge
           :initarg :merge
           :initform #'identity)
   ;; Returns if any data was merged
   (%spawn :accessor spawn
           :initarg :spawn
           :initform #'identity)
   ;; Returns the amount of spawned threads
   (%done :accessor done
          :initarg :done
          :initform (constantly nil))
   ;; Return value is ignored
   ))

(defun make-step (name fetch merge spawn done)
  (make-instance 'step :name name
                       :fetch fetch :merge merge
                       :spawn spawn :done done))

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
        (let ((data (apply (fetch step) initial-data)))
          (debug "fetched")
          (when (apply (merge step) data)
            (debug "merged"))
          (loop with format-spawn = "~D tasks spawned for step ~A"
                with steps = (next-steps step)
                for step in steps
                for n = (apply (spawn step) step data)
                unless (= 0 n)
                  do (debug (format nil format-spawn n step)))
          (apply (done step) data)
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

(defun run-flow (flow-name &rest initial-data)
  (let* ((*current-flow* flow-name)
         (*step-type* "Synchronization step")
         (step (gethash *flow-steps* flow-name)))
    ))










(defclass flow ()
  ((%name :accessor name
          :initarg :name
          :initform (error "Must provide NAME."))
   (%prepare-fn :accessor prepare-fn
                :initarg :prepare-fn
                :initform (lambda (&rest x) x))
   (%waves :accessor waves
           :initarg :waves
           :initform (make-hash-table))
   (%first-wave :accessor first-wave
                :initarg :first-wave
                :initform nil)
   (%next-waves :accessor next-waves
                :initarg :next-waves
                :initform (make-hash-table))))

(defun make-flow (name prepare-fn first-wave next-waves)
  (make-instance 'flow :name name :prepare-fn prepare-fn
                       :first-wave first-wave :next-waves next-waves))

(defvar *flows* (make-hash-table))

(defvar *current-flow*)

(defmacro define-flow (name options &body forms)
  (let ((first-wave (caar forms))
        (prepare-fn (getf options :prepare-fn))
        (next-waves (mapcar (curry #'remove '->) forms)))
    (with-gensyms (instance)
      `(let ((,instance (make-flow ',name ,prepare-fn
                                   ',first-wave ',next-waves)))
         (setf (gethash ',name *flows*) ,instance)
         ',name))))

(define-flow synchronize (:prepare-fn 'synchronize-prepare)
  (login            -> download-account)
  (download-account -> download-furres
                    -> download-images)
  (download-furres  -> download-costumes
                    -> download-portraits
                    -> download-specitags))

(defun synchronize-prepare (picker emails-and-passwords)
  (mapcar (lambda (x) (list (first x) (second x) picker))
          emails-and-passwords))

(define-step (synchronize login)
  :fetch #'login-fetch
  :merge #'login-merge
  :spawn #'login-spawn
  :done #'login-done)

(defun login-fetch (email password picker)
  (let ((cookie-jar (cl-furcadia/ws:login email password)))
    (unless cookie-jar
      (error "Invalid credentials for amail ~A." email))
    `((,email ,cookie-jar ,picker))))

(defun login-merge (email cookie-jar picker)
  (with-lock-held ((lock picker))
    (setf (gethash email (emails-cookie-jars picker)) cookie-jar)
    t))

(defun login-spawn (step email cookie-jar picker)
  (let ((fn (curry #'call-step step email cookie-jar picker))
        (name (format nil "RL account thread: ~A" email)))
    (push-queue (make-thread fn :name name) (queue picker))
    1))

(defun login-done (email cookie-jar picker)
  (declare (ignore cookie-jar picker))
  (signal! picker (login-completed string) email))
