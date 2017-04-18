;;;; config.lisp

(in-package :furcadia-launcher)

;;;;;; CONFIGURATION - EDIT THESE

(defvar *email* "") ;; no need to edit these - if you forget to,
(defvar *password* "") ;; the program will ask you to log in.
(defvar *furcadia-path* "C:\\Furcadia\\") ;; remember double-slashes here

(defvar *cookie-jar* (make-instance 'cookie-jar))
(defparameter *secret* "")
(defparameter *secret-fured* "")
(defvar *save-char-keywords*
  '("colr" "desc" "digo" "wing" "port" "tag" "adesc" "awhsp" "aresp"
    "doresp" "adigo" "awing" "atime" "amaxtime" "acolr" "aport" "uid"))

(defun input-login-data ()
  (format t "Email:    ")
  (setf *email* (read-line))
  (format t "Password: ")
  (setf *password* (read-line)))
