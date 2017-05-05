;;;; backend-furcadia.lisp

(in-package :furcadia-launcher)

(defun generate-furcadia-command (furcadia-path login-link)
  "Generates a Furcadia command suitable for use with UIOP:LAUNCH-PROGRAM,
dependent on the provided Furcadia path and login link."
  (if (uiop:os-windows-p)
      (windows-furcadia-command furcadia-path login-link)
      (linux-furcadia-command furcadia-path login-link))
  ;; (let* ((fixed-path
  ;;          (cat "\""
  ;;               (replace-all (princ-to-string (truename furcadia-path))
  ;;                            "/" "\\")
  ;;               "\""))
  ;;        (exe-path (princ-to-string (truename (cat furcadia-path
  ;;                                                  "Furcadia.exe"))))
  ;;        (path fixed-path)
  ;;        (command (list "/c" "start" "\"Furcadia\""
  ;;                       (replace-all (cat "\"" exe-path "\"")
  ;;                                    "/" "\\")))
  ;;        (result (append command
  ;;                        (list "-defaultpath" path
  ;;                              "-followurl" login-link))))
  ;;   (format nil "~{~A~^ ~}" result))
  )

(defun windows-furcadia-command (furcadia-path login-link &key im-sure)
  "Generates a command suitable for running Furcadia on Windows through CMD."
  ;; Wait, what the hell - why CMD?
  ;;
  ;; It turns out that, depending on how the Lisp process (the parent of the
  ;; Furcadia process) is spawned, the Furcadia's main window might or might
  ;; *not* be shown by the OS when it first calls the Windows API's ShowWindow
  ;; function.
  ;; This is why we make a workaround - we actually decide to use the shell
  ;; functionality of UIOP:LAUNCH-PROGRAM and invoke CMD with a START command.
  ;; The CMD-in-the-middle is the one that launches the proper Furcadia process
  ;; and can guarantee that the ShowWindow will always succeed.
  ;; In order to coerce UIOP:LAUNCH-PROGRAM to use CMD, we must print the whole
  ;; command as a string, therefore this function returns a string with the full
  ;; command.
  ;;
  ;; See the below link for the official MS docs. (Then switch to a sane OS.)
  ;; https://msdn.microsoft.com/en-us/library/windows/desktop/ms686331.aspx
  ;;
  ;;                   wwww                                      wwww
  ;;                     ^^##                                      ^^##
  ;;                     ww##                                      ww##
  ;;     ##################^^                      ##################^^
  ;;       ww############ww                          ww############ww
  ;;   ww####^^        ^^####ww                  ww####^^        ^^####ww
  ;;   ####                ####                  ####                ####
  ;;   ####      ####      ####                  ####      ####      ####
  ;;   ####      ####      ####                  ####      ####      ####
  ;;   ####                ####                  ####                ####
  ;;     ####ww        ww####                      ####ww        ww####
  ;;       ^^############^^                          ^^############^^
  ;;
  ;;
  ;;                       ##########################
  ;;
  (unless (uiop:os-windows-p)
    (warn "WINDOWS-FURCADIA-COMMAND is not safe to call outside Windows ~
because non-Windows implementations seemingly lack the ability to process ~
Windows paths. Use :IM-SURE T to override.")
    (unless im-sure (return-from windows-furcadia-command)))
  (let* ((exe-path (uiop:native-namestring (cat furcadia-path "Furcadia.exe")))
         (dir-path (uiop:native-namestring furcadia-path))
         (result (list "/c" "start" "\"Furcadia\"" exe-path
                       "-defaultpath" dir-path
                       "-followurl" login-link)))
    (format nil "~{~A~^ ~}" result)))

(defun linux-furcadia-command (furcadia-path login-link)
  "Generates a command suitable for running Furcadia on Linux through Wine."
  ;; Our method for Linux - we utilize winepath to generate Windows paths
  ;; for both Furcadia and its directory and then launch wine with the full
  ;; path constructed. This function returns a list, so UIOP:LAUNCH-PROGRAM does
  ;; not launch a shell process.
  (let ((exe-path (winepath (cat furcadia-path "Furcadia.exe")))
        (dir-path (winepath furcadia-path)))
    (list "wine" exe-path
          "-defaultpath" dir-path
          "-followurl" login-link)))

(defun winepath (path)
  "Calls the Wine utility WINEPATH on the provided unix path."
  (trim-whitespace
   (with-output-to-string (*standard-output*)
     (uiop:run-program (list "winepath" "-w" path)
                       :output t))))

(defun launch-furcadia (furcadia-path login-link)
  "Launches Furcadia, using the provided Furcadia path and login link."
  (let ((command (generate-furcadia-command furcadia-path login-link)))
    (uiop:chdir furcadia-path)
    (uiop:launch-program command)))
