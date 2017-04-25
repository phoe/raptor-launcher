;;;; worker-login.lisp

(in-package :furcadia-launcher)

(defun make-login-cons (email password)
  "Provided an account, returns a cons, in which CAR is the email and CDR is a
worker thread which executes the login for the provided account."
  ;; TODO handle errors in threads
  (cons email (make-thread (curry #'do-login email password)
                           :name (cat "Login worker for " email))))

(defun make-fured-cons (cons)
  "Provided a cons, whose CAR is email and CDR is a cookie jar, returns a cons,
whose CAR is the email and CDR is a worker thread which fetches the FurEd
page for the provided cookie jar."
  ;; TODO handle errors in threads
  (destructuring-bind (email . cookie-jar) cons
    (cons email (make-thread (curry #'http-get-fured-page cookie-jar)
                             :name (cat "FurEd worker for " email)))))

(defun make-load-character-cons (cons)
  "Provided a cohs, whose CAR is a shortname and CDR is a cookie jar, returns a
cons, whose CAR is the shortname and CDR is a worker thread which loads the
character JSON for the provided shortname and cookie jar."
  (destructuring-bind (sname . cookie-jar) cons
    (cons sname (make-thread (compose #'decode-character
                                      (curry #'http-load-character sname cookie-jar))
                             :name (cat "Load-char worker for " sname)))))

(defun login-all (&optional (config *config*))
  ;; TODO add key unlogged-only
  "Logs in all accounts in the given config in parallel and returns an alist, in
which the keys are the logins and values are the respective cookie jars.
This list is suitable for a call to (SETF STATE-COOKIES) of of any given state
of the launcher."
  (let* ((accounts (getf config :accounts))
         (alist (sleepcar (curry #'apply #'make-login-cons) accounts))
         (values (finalize-thread-alist alist)))
    values))

(defmacro login-allf (&optional (config '*config*) (state '*state*)
                        (state-lock '*state-lock*))
  "Modify macro for LOGIN-ALL that automatically calls SETF STATE-COOKIES on the
provided STATE."
  `(setf (state-cookies ,state ,state-lock) (login-all ,config)))
