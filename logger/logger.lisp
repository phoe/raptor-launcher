;;;; logger.lisp
;;;; Adapted from phoe/gateway

(in-package :furcadia-launcher)

(defclass logger ()
  ((%thread :accessor thread)
   (%name :accessor name)
   (%stream :accessor stream-of)
   (%queue :accessor queue
           :initform (make-queue)))
  (:documentation "The logger class, borrowed from Gateway and adapted."))

(defmethod initialize-instance :after ((logger logger) &key)
  (let ((name "Furcadia Launcher logger")
        (setter (lambda () (setf (stream-of logger) *standard-output*))))
    (join-thread (make-thread setter))
    (setf (name logger) name
          (thread logger)
          (make-thread (curry #'logger-loop logger) :name name))))

(defun logger-loop (logger)
  (let ((*print-right-margin* most-positive-fixnum))
    (loop
      (apply #'fformat (stream-of logger) (cdr (pop-queue (queue logger))))
      (fresh-line))))

(defun note (type &rest args)
  (%note type args))

(defun %note (type args)
  (when (and (boundp '*logger*) *logger* (alivep *logger*))
    (push-queue (cons type args) (queue *logger*))))

(defmethod alivep ((logger logger))
  (thread-alive-p (thread logger)))

(defmethod kill ((logger logger))
  (unless (eq (current-thread) (thread logger))
    (destroy-thread (thread logger)))
  (values))

(defvar *logger* (make-instance 'logger)
  "The current logger.")
