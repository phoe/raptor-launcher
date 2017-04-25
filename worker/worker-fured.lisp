;;;; worker-fured.lisp

(in-package :furcadia-launcher)

(defun fetch-all-accounts (&optional (state *state*) (state-lock *state-lock*))
  "Provided a state and a state lock, returns a list of decoded account JSONs."
  (let* ((cookie-alist (state-cookies state state-lock))
         (fured-alist (sleepcar #'make-fured-cons cookie-alist))
         (fured-pages (mapcar #'cdr (finalize-thread-alist fured-alist)))
         (accounts (mapcar #'extract-fured-account-json fured-pages)))
    (dolist (account accounts)
      (assert (assoc :email account)))
    accounts))

(defmacro fetch-all-accountsf (&optional (state '*state*)
                                 (state-lock '*state-lock*))
  "Modify macro for FETCH-ALL-ACCOUNTS that automatically sets SETF
STATE-ACCOUNTS on the provided STATE."
  `(setf (state-accounts ,state ,state-lock)
         (fetch-all-accounts ,state ,state-lock)))

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

(defun fetch-all-characters (&optional (state *state*)
                               (state-lock *state-lock*))
  "Provided a list of decoded account JSONs, a state and a state-lock, accesses
the accounts stored in the state, fetches all characters on the provided
accounts in parallel and returns an alist, in which CARs are the character
shortnames and CDRs are the decoded character JSONs."
  (let* ((accounts (state-accounts state state-lock))
         (email-shortnames (mapcan #'make-email-shortname-alist accounts))
         (email-cookies (state-cookies state state-lock))
         (shortname-cookies (shortnames-cookies email-shortnames email-cookies))
         (alist (sleepcar #'make-load-character-cons shortname-cookies))
         (character-list (finalize-thread-alist alist)))
    character-list))

(defmacro fetch-all-charactersf (&optional (config '*config*) (state '*state*)
                                   (state-lock '*state-lock*))
  "Modify macro for FETCH-ALL-CHARACTERS which automatically calls
SETF (GETF CONFIG :CHARACTERS) on the provided config."
  ;; TODO write SETF CONFIG-CHARACTERS
  `(setf (getf ,config :characters)
         (fetch-all-characters ,state ,state-lock)))

(defun character-login-link (sname &optional (config *config*)
                                     (state *state*) (state-lock *state-lock*))
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
