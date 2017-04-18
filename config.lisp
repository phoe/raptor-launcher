;;;; config.lisp

(in-package :furcadia-launcher)

;;;;;; CONFIGURATION - EDIT THESE

(defvar *furcadia-path* "C:\\Furcadia\\") ;; remember double-slashes here

(defvar *cookie-jar* (make-instance 'cookie-jar))
(defparameter *secret* "")
(defparameter *secret-fured* "")
(defvar *save-char-keywords*
  '("colr" "desc" "digo" "wing" "port" "tag" "adesc" "awhsp" "aresp"
    "doresp" "adigo" "awing" "atime" "amaxtime" "acolr" "aport" "uid"))

(defvar *config* nil)
(defvar *config-path* "~/.furcadia-launcher/config.lisp")

(defvar *sample-config*
  '(:master-password nil
    :accounts (("foo@bar.com" "FooBar1234")
               ("frob@mem.org" "Frobnicate!@#4"))
    ))

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
