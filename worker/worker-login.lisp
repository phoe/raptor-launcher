;;;; worker-login.lisp

(in-package :furcadia-launcher)

(defun make-login-thread (email password)
  "Returns a worker thread which executes the login for the provided email and
password."
  ;; TODO handle errors in threads
  (make-thread (curry #'do-login email password)
               :name (cat "Login worker for " email)))

(defun make-thread-alist (accounts function &optional (n 5) (sleep-interval 1))
  "Provided a list of accounts, returns an alist, whose CARs are emails and CDRs
are worker threads executing the login for the particular email. This function
additionally sleeps for INTERVAL seconds every N requests in order to prevent
DDoS protection on the server from triggering."
  (flet ((construct (account)
           (destructuring-bind (email password) account
             (cons (first account) (funcall function email password)))))
    (mapcar-sleep-every #'construct accounts n sleep-interval)))

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
  "Logs in all accounts in the given config and returns an alist, in which the
keys are the logins and values are the respective cookie jars. This list is
suitable for a call to SETF of (GETHASH :COOKIE-JARS STATE) of any given state
of the launcher."
  (let* ((accounts (getf config :accounts))
         (alist (make-thread-alist accounts #'make-login-thread))
         (values (finalize-thread-alist alist)))
    values))
