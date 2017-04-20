;;;; config.lisp

(in-package :furcadia-launcher)

(defvar *config* nil)
(defvar *config-path* "~/.furcadia-launcher/config.lisp")

(defvar *sample-config*
  '(:furcadia-path "/home/foo/.wine/drive_c/Furcadia/"
    :accounts (("foo@bar.com" "FooBar1234")
               ("frob@mem.org" "Frobnicate!@#4"))))

(defun load-config-file ()
  (ensure-directories-exist *config-path*)
  (with-open-file (s *config-path*
                     :direction :input
                     :if-does-not-exist :create)
    (read s nil nil)))

(defun save-config-file (&optional (config *config*))
  (ensure-directories-exist *config-path*)
  (with-open-file (s *config-path*
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists :overwrite)
    (print config s)
    (terpri s)))
