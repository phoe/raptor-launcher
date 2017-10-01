;;;; logger.lisp
;;;; Adapted from phoe/gateway

(in-package :furcadia-launcher)

;;;; HOOKS

(defparameter *log-dir* (merge-pathnames ".furcadia-launcher/logs/"
                                         (user-homedir-pathname)))
(defparameter *logger-filename*
  (merge-pathnames (format nil "logger-~A.txt" (get-unix-time)) *log-dir*))

(defun logger-console-hook (type &rest args)
  (when (and (boundp '*logger*) *logger* (alivep *logger*))
    (format (stream-of *logger*) "[~5A] " type)
    (apply #'fformat (stream-of *logger*) args)
    (fresh-line (stream-of *logger*))))

(defun logger-file-hook (type &rest args)
  (setf *log-dir* (merge-pathnames ".furcadia-launcher/logs/"
                                   (user-homedir-pathname)))
  (setf *logger-filename* (merge-pathnames (format nil "logger-~A.txt"
                                                   (get-unix-time)) *log-dir*))
  (ensure-directories-exist *log-dir*)
  (with-open-file (stream *logger-filename*
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
    (format stream "[~A] " (get-unix-time))
    (format stream "[~5A] " type)
    (apply #'fformat stream args)
    (fresh-line stream)))

;;;; IMPLEMENTATION

(defclass logger ()
  ((%thread :accessor thread)
   (%name :accessor name)
   (%stream :accessor stream-of)
   (%queue :accessor queue
           :initform (make-queue)))
  (:documentation "The logger class, borrowed from Gateway and adapted."))

(defmethod initialize-instance :after ((logger logger) &key)
  (setf *log-dir* (merge-pathnames ".furcadia-launcher/logs/"
                                   (user-homedir-pathname)))
  (let ((name "Furcadia Launcher logger")
        (setter (lambda () (setf (stream-of logger) *standard-output*))))
    (join-thread (make-thread setter))
    (setf (name logger) name
          (thread logger)
          (make-thread (curry #'logger-loop logger) :name name))
    (push-queue (list :info "Logger started on ~A." (get-unix-time))
                (queue logger))))

(defun logger-loop (logger)
  (let ((*print-right-margin* most-positive-fixnum))
    (loop
      (let ((data (pop-queue (queue logger))))
        (mapc (rcurry #'apply data) *logger-hooks*)))))

(defparameter *logger-hooks*
  (list ;;'logger-console-hook
   'logger-file-hook))

(defmethod alivep ((logger logger))
  (thread-alive-p (thread logger)))

(defmethod kill ((logger logger))
  (unless (eq (current-thread) (thread logger))
    (destroy-thread (thread logger)))
  (values))

(defun note (type &rest args)
  (%note type args))

(defun %note (type args)
  (when (and (boundp '*logger*) *logger* (alivep *logger*))
    (push-queue (cons type args) (queue *logger*))
    nil))

(defvar *logger* (make-instance 'logger)
  "The current logger.")

(defun restart-logger ()
  (kill *logger*)
  (setf *logger* (make-instance 'logger)))
