;;;; build.lisp

(defun logger-build-hook ()
  (furcadia-launcher::kill furcadia-launcher::*logger*)
  (loop until (not (furcadia-launcher::alivep furcadia-launcher::*logger*))
        do (sleep 0.1)))

(defun logger-boot-hook ()
  (setf furcadia-launcher::*logger*
        (make-instance 'furcadia-launcher::logger)))

(pushnew 'logger-build-hook qtools:*build-hooks*)

(pushnew 'logger-boot-hook qtools:*boot-hooks*)
