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

(defun finalize-thread-alist (alist)
  "Provided an alist, in which CDRs are threads, blocks and waits for each
thread to finish before destructively replacing each CDR with the resulting
value of the thread's function."
  ;; We assume that JOIN-THREAD returns the value of the function the thread
  ;; was called with. In the event of BORDEAUX-THREADS being fixed in order
  ;; to provide a documented function that returns the return value of the
  ;; thread by *definition*, not by *accident*, please fix this code to use
  ;; the new function. In the event an implementation does not conform to
  ;; this behaviour, please fix your impleme^W^W^Wcreate a bug on this
  ;; project.
  (flet ((finalize-cdr (cons)
           (setf (cdr cons) (join-thread (cdr cons)))))
    (mapc #'finalize-cdr alist)))

(defun login-all (config)
  ;; TODO add key unlogged-only
  "Logs in all accounts in the given config in parallel and returns an alist, in
which the keys are the logins and values are the respective cookie jars.
This list is suitable for a call to SETF of (GETHASH :COOKIE-JARS STATE) of any
given state of the launcher."
  (let* ((accounts (getf config :accounts))
         (alist (sleepcar (curry #'apply #'make-login-cons) accounts))
         (values (finalize-thread-alist alist)))
    values))

(defun fetch-all-accounts (state state-lock)
  "Provided a state and a state lock, returns a list of decoded account JSONs."
  (let* ((cookie-alist (state-cookies state state-lock))
         (fured-alist (sleepcar #'make-fured-cons cookie-alist))
         (fured-pages (mapcar #'cdr (finalize-thread-alist fured-alist)))
         (accounts (mapcar #'extract-fured-account-json fured-pages)))
    (dolist (account accounts)
      (assert (assoc :email account)))
    accounts))

(defun make-email-shortname-alist (account)
  "Provided an account, returns an alist whose CARs are the email bound to that
account and CDRs are successive characters on that "
  (let ((characters (list-characters account))
        (email (cdr (assoc :email account))))
    (mapcar (lambda (x) (cons email x)) characters)))

(defun shortnames-cookies (email-shortname-alist email-cookie-alist)
  "Provided an alist of emails and shortnames and an alist of emails and
cookie jars, returns an alist of shortnames and cookie jars."
  (flet ((construct (x)
           (cons (cdr x)
                 (assoc-value email-cookie-alist (car x) :test #'string=))))
    (mapcar #'construct email-shortname-alist)))

(defun fetch-all-characters (accounts state state-lock)
  "Provided a list of decoded account JSONs, a state and a state-lock, fetches
all characters on the provided accounts in parallel and returns an alist, in
which CARs are the character shortnames and CDRs are the decoded character
JSONs."
  (let* ((email-shortnames (mapcan #'make-email-shortname-alist accounts))
         (email-cookies (state-cookies state state-lock))
         (shortname-cookies (shortnames-cookies email-shortnames email-cookies))
         (alist (sleepcar #'make-load-character-cons shortname-cookies))
         (values (finalize-thread-alist alist)))
    values))

(defun character-login-link (sname config state state-lock)
  "Provided a shortname, a config, a state and a state lock, returns a furc://
login link for the character with the respective shortname."
  (let* ((character (cdr (assoc sname (getf config :characters)
                                :test #'string=)))
         (accounts (with-lock-held (state-lock) (gethash :accounts state)))
         (email-shortnames (mapcan #'make-email-shortname-alist accounts))
         (email (rassoc-value email-shortnames sname :test #'string=))
         (account (find email accounts :test #'string=
                                       :key (rcurry #'assoc-value :email)))
         (fured-secret (assoc-value account :session))
         (cookie-jar (state-cookie state state-lock email))
         (result (http-save-character character cookie-jar fured-secret)))
    (extract-login-link result)))
