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
  (let* ((exe-path (cat furcadia-path "Furcadia.exe"))
         (path (if (uiop:os-windows-p)
                   furcadia-path
                   (winepath furcadia-path)))
         (command (if (uiop:os-windows-p)
                      (list exe-path)
                      (list "wine" (winepath exe-path)))))
    (append command
            (list "-defaultpath" path
                  "-followurl" login-link))))

(defun launch-furcadia (furcadia-path login-link)
  "Launches Furcadia, using the provided Furcadia path and login link."
  (let ((command (generate-furcadia-command furcadia-path login-link)))
    (uiop:chdir furcadia-path)
    (uiop:launch-program command)))
