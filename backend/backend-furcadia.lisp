;;;; backend-furcadia.lisp

(in-package :furcadia-launcher)

(defun winepath (path)
  "Calls the Wine utility WINEPATH on the provided unix path."
  (trim-whitespace
   (with-output-to-string (*standard-output*)
     (uiop:run-program (list "winepath" "-w" path)
                       :output t))))

(defun generate-furcadia-command (furcadia-path login-link)
  "Generates a Furcadia command suitable for use with UIOP:LAUNCH-PROGRAM,
dependent on the provided Furcadia path and login link."
  ;;;; WINDOWS IS AN ABOMINATION
  (let* ((fixed-path
           (cat "\""
                (replace-all (princ-to-string (truename furcadia-path))
                             "/" "\\")
                "\""))
         (exe-path (princ-to-string (truename (cat furcadia-path
                                                   "Furcadia.exe"))))
         (path (if (uiop:os-windows-p)
                   fixed-path
                   (winepath furcadia-path)))
         (command (if (uiop:os-windows-p)
                      (list "/c" "start" "\"\""
                            (replace-all (cat "\"" exe-path "\"")
                                         "/" "\\"))
                      (list "wine" (winepath exe-path))))
         (result (append command
                         (list "-defaultpath" path
                               "-followurl" login-link))))
    (if (uiop:os-windows-p)
        (format nil "~{~A~^ ~}" result)
        result)))

(defun launch-furcadia (furcadia-path login-link)
  "Launches Furcadia, using the provided Furcadia path and login link."
  (let ((command (generate-furcadia-command furcadia-path login-link)))
    (uiop:chdir furcadia-path)
    (uiop:launch-program command)))
