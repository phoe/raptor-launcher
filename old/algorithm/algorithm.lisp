;;;; algorithm-login.lisp

(in-package :furcadia-launcher)

(defun error-message-load-config (condition)
  (format nil "An error of type ~A occurred while reading the config file. ~
Check that the configuration file exists and is in the proper syntax."
          (type-of condition)))

(defun error-message-credentials () nil
  "Account credentials are missing or improperly set in config.")

(defun error-login-all-accounts (condition) nil
  (format nil "There was an error of type ~A while logging your accounts in. ~
Make sure that that the network connection is not interrupted."
          (type-of condition)))

(defun error-fetch-all-accounts (condition) nil
  (format nil "There was an error of type ~A while fetching your account data. ~
Make sure that that the network connection is not interrupted."
          (type-of condition)))

(defun error-fetch-all-characters (condition) nil
  (format nil "There was an error of type ~A while fetching your character ~
data. Make sure that that the network connection is not interrupted."
          (type-of condition)))

(defun error-message-save-config (condition)
  (format nil "An error of type ~A occurred while saving the config file. ~
Check that the configuration file is writable."
          (type-of condition)))

(defun algorithm-load-config-file ()
  (note :info "Loading configuration file.")
  (handler-case
      (progn
        (setf *config* (load-config-file))
        t)
    (error (e)
      (note :error (error-message-load-config e)))))

(defun algorithm-verify-config-file ()
  (note :info "Verifying config file.")
  (let ((accounts (getf *config* :accounts)))
    (cond ((or (null accounts)
               (some (lambda (x) (< (length x) 2)) accounts)
               (some (compose #'not #'stringp)
                     (apply #'append accounts))
               (some (curry #'string= "")
                     (apply #'append accounts)))
           (note :error (error-message-credentials)))
          (t
           (note :info "Credentials seem sane - ready for initialization.")
           t))))

(defun algorithm-login-all-accounts ()
  (note :info "Logging accounts in.")
  (handler-case
      (when (every #'cdr (login-allf))
        (note :info "All ~D accounts logged in successfully."
              (length (state-cookies *state* *state-lock*)))
        t)
    (error (e)
      (note :error (error-login-all-accounts e)))))

(defun algorithm-fetch-all-accounts ()
  (note :info "Fetching account data.")
  (handler-case
      (progn
        (fetch-all-accountsf)
        (note :info "Data for all ~D accounts fetched successfully."
              (length (state-cookies *state* *state-lock*)))
        t)
    (error (e)
      (note :error (error-fetch-all-accounts e)))))

(defun algorithm-fetch-all-characters ()
  (note :info "Fetching character data.")
  (handler-case
      (progn
        (fetch-all-charactersf)
        (note :info "Data for all ~D characters fetched successfully."
              (length (getf *config* :characters)))
        t)
    (error (e)
      (note :error (error-fetch-all-characters e)))))

(defun algorithm-save-config-file ()
  (note :info "Saving configuration file.")
  (handler-case
      (progn
        (save-config-file)
        t)
    (error (e)
      (note :error (error-message-save-config e)))))

(defun initialize ()
  (when (and (algorithm-load-config-file)
             (algorithm-verify-config-file)
             (algorithm-login-all-accounts)
             (algorithm-fetch-all-accounts)
             (algorithm-fetch-all-characters)
             (algorithm-save-config-file))
    (note :info "Config file saved.")
    (note :info "Initialization complete.")
    t))
