;;;; state.lisp

(in-package :furcadia-launcher)

(defvar *state* (make-hash-table)
  "The current running state of the program.")

(defvar *state-lock* (make-lock "Furcadia Launcher state")
  "The lock for the current running state of the program.")

(defun state-cookie (state state-lock email)
  "Returns the cookie jar for a given email in the provided state."
  (with-lock-held (state-lock)
    (let ((cookie-jars (gethash :cookie-jars state)))
      (assoc-value cookie-jars email :test #'string=))))

(defun (setf state-cookie) (new-value state state-lock email)
  "Sets the cookie jar for a given email in the provided state."
  (with-lock-held (state-lock)
    (symbol-macrolet ((cookie-jars (gethash :cookie-jars state)))
      (setf (assoc-value cookie-jars email :test #'string=) new-value))))

(defun state-cookies (state state-lock)
  "Returns an alist of all emails and cookie jars in the provided state."
  (with-lock-held (state-lock)
    (gethash :cookie-jars state)))

(defun (setf state-cookies) (new-value state state-lock)
  "Sets the alist of all emails and cookie jars in the provided state."
  (with-lock-held (state-lock)
    (setf (gethash :cookie-jars state) new-value)))
