;;;; build.lisp

(in-package :furcadia-launcher)

(defun main-build-hook ()
  (setf drakma:*drakma-default-external-format* :utf-8)
  #+sbcl (setf *debugger-hook*
               (lambda (c x)
                 (declare (ignore c x))
                 (sb-debug:print-backtrace :count most-positive-fixnum))))

(defun logger-build-hook ()
  (kill *logger*)
  (loop until (not (alivep *logger*))
        do (sleep 0.1)))

(defun logger-boot-hook ()
  (setf *logger* (make-instance 'logger)))

(pushnew 'main-build-hook qtools:*build-hooks*)

(pushnew 'logger-build-hook qtools:*build-hooks*)

(pushnew 'logger-boot-hook qtools:*boot-hooks*)
