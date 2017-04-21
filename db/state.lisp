;;;; state.lisp

(in-package :furcadia-launcher)

(defvar *state* nil
  "The current running state of the program.")

(defvar *state-lock* (make-lock "Furcadia Launcher state")
  "The lock for the current running state of the program.")

(defun config-cookie (config email)
  "Returns the cookie jar for a given email."
  (let ((cookie-jars (assoc :cookie-jars config :test #'string=)))
    (assoc email cookie-jars :test #'string=)))
