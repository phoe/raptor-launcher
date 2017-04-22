;;;; logger.lisp

(in-package :furcadia-launcher)

(defclass logger ()
  ((%thread :accessor thread)
   (%name :accessor name)
   (%stream :accessor stream-of)
   (%queue :accessor queue
           :initform (make-queue))))

(defmethod initialize-instance :after ((logger logger) &key)
  (let ((name "Launcher - Logger")
        (setter (lambda () (setf (stream-of logger) *standard-output*))))
    (join-thread (make-thread setter))
    (setf (name logger) name
          (thread logger)
          (make-thread (curry #'%logger-loop logger) :name name))))

(defun %logger-loop (logger)
  (let ((*print-right-margin* most-positive-fixnum))
    (apply #'fformat (stream-of logger) (pop-queue (queue logger)))
    (fresh-line)))

(defun %note (args)
  (when (and (boundp '*logger*) *logger* (alivep *logger*))
    (push-queue args (queue *logger*))))

(defmethod alivep ((logger logger))
  (thread-alive-p (thread logger)))

(defmethod kill ((logger logger))
  (unless (eq (current-thread) (thread logger))
    (destroy-thread (thread logger)))
  (values))

(setf *logger* (make-instance 'logger))
