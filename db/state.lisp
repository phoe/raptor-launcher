;;;; state.lisp

(in-package :furcadia-launcher)

(defvar *state* (make-hash-table)
  "The current running state of the program.")

(defvar *state-lock* (make-lock "Furcadia Launcher state")
  "The lock for the current running state of the program.")

;;;; STATE-COOKIE

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

;;;; STATE-ACCOUNT

(defun state-account (state state-lock email)
  "Returns the account for a given email in the provided state."
  (with-lock-held (state-lock)
    (let ((accounts (gethash :accounts state)))
      (find email accounts :test #'string=
                           :key (rcurry #'assoc-value :email)))))

(defun (setf state-account) (new-value state state-lock email)
  "Sets the account for a given email in the provided state."
  (with-lock-held (state-lock)
    (let ((accounts (remove email (gethash :accounts state)
                            :test #'string=
                            :key (rcurry #'assoc-value :email))))
      (setf (gethash :accounts state) (cons new-value accounts)))))

(defun state-accounts (state state-lock)
  "Returns all accounts in the provided state."
  (with-lock-held (state-lock)
    (gethash :accounts state)))

(defun (setf state-accounts) (new-value state state-lock)
  "Sets all accounts in the provided state."
  (with-lock-held (state-lock)
    (setf (gethash :accounts state) new-value)))

;;;; STATE-CHARON-INFO


(defun state-charon-info (state state-lock email)
  "Returns the Charon information for a given email in the provided state."
  (with-lock-held (state-lock)
    (let ((cookie-jars (gethash :charon-infos state)))
      (assoc-value cookie-jars email :test #'string=))))

(defun (setf state-charon-info) (new-value state state-lock email)
  "Sets the Charon information for a given email in the provided state."
  (with-lock-held (state-lock)
    (symbol-macrolet ((cookie-jars (gethash :charon-infos state)))
      (setf (assoc-value cookie-jars email :test #'string=) new-value))))

(defun state-charon-infos (state state-lock)
  "Returns an alist of all emails and Charon informations in the provided
state."
  (with-lock-held (state-lock)
    (gethash :charon-infos state)))

(defun (setf state-charon-infos) (new-value state state-lock)
  "Sets the alist of all emails and Charon informations in the provided state."
  (with-lock-held (state-lock)
    (setf (gethash :charon-infos state) new-value)))


;;;; STATE-LAST-LOGINS

(defun state-last-logins (&optional (state *state*)
                            (state-lock *state-lock*))
  "Returns the alist of names, shortnames and login dates of the provided
state."
  (with-lock-held (state-lock)
    (gethash :last-logins state)))

(defun (setf state-last-logins) (new-value state state-lock)
  "Sets the alist of names, shortnames and login dates of the provided
state."
  (with-lock-held (state-lock)
    (setf (gethash :last-logins state) new-value)))
