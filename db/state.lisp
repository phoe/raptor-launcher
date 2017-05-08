;;;; state.lisp

(in-package :furcadia-launcher)

(defvar *state-path* "~/.furcadia-launcher/state.lisp"
  "The location of the saved state file.")

(defvar *state* (make-hash-table)
  "The current running state of the program.")

(defvar *state-lock* (make-lock "Furcadia Launcher state")
  "The lock for the current running state of the program.")

;;;; STORING

(defun serialize-state (&optional (state *state*) (state-lock *state-lock*))
  "Provided a state, returns a serialized state, suitable for storing in a file
as a PRINTed value."
  (let ((plist (with-lock-held (state-lock) (hash-table-plist state))))
    (loop for (key value) on plist by #'cddr
          collect key
          if (eq key :cookie-jars)
            collect (serialize-cookie-jars value)
          else collect value)))

(defun deserialize-state (plist)
  "Provided a serialized state, returns a state, suitable for storing in the
*STATE* variable."
  (loop for (key value) on plist by #'cddr
        collect key
        if (eq key :cookie-jars)
          collect (deserialize-cookie-jars value)
        else collect value))

(defun save-state-file (&optional (state *state*) (state-lock *state-lock*))
  "Saves the current state to disk. If no state is provided, the value of
*STATE* is used."
  (ensure-directories-exist *state-path*)
  (let ((result (serialize-state state state-lock)))
    (with-open-file (s *state-path*
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede)
      (print result s)
      (terpri s)
      (note :info "State saved to disk.")
      t)))

(defun load-state-file ()
  "Reads and returns the state file from disk.
Note that this function does not set the file to the *STATE* var - you need to
do it manually through ."
  (ensure-directories-exist *state-path*)
  (let* ((plist (with-open-file (s *state-path*
                                   :direction :input
                                   :if-does-not-exist :create)
                  (read s nil nil)))
         (hashtable (plist-hash-table (deserialize-state plist))))
    (note :info "State loaded from disk.")
    hashtable))

(defun valid-state-p (state)
  "Returns true if STATE is a valid state, and false otherwise."
  (and (nth-value 1 (gethash :last-logins state))
       (nth-value 1 (gethash :accounts state))
       (nth-value 1 (gethash :cookie-jars state))))

;;;; TODO refactor all of these below so STATE and STATE-LOCK are at the end and
;;;; optional, bound to *STATE* and *STATE-LOCK*

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
