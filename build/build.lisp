;;;; build.lisp

(in-package :furcadia-launcher)

(defun main-build-hook ()
  (setf drakma:*drakma-default-external-format* :utf-8)
  #+sbcl
  (setf *debugger-hook*
        (lambda (c x)
          (declare (ignore x))
          (let ((backtrace (with-output-to-string (s)
                             (sb-debug:print-backtrace
                              :count most-positive-fixnum))))
            (note :error "Fatal error: ~A~%Backtrace:~%~A" c backtrace)
            (sb-ext:exit :code 1)))))

(defun logger-build-hook ()
  (kill *logger*)
  (loop until (not (alivep *logger*))
        do (sleep 0.1)))

(defun logger-boot-hook ()
  (setf *logger* (make-instance 'logger)))

(pushnew 'main-build-hook qtools:*build-hooks*)

(pushnew 'logger-build-hook qtools:*build-hooks*)

(pushnew 'logger-boot-hook qtools:*boot-hooks*)
