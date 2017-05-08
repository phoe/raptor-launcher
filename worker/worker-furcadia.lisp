;;;; worker-furcadia.lisp

(in-package :furcadia-launcher)

(defun error-message-furcadia-path () nil
  "Furcadia path is missing or improperly set in config. Check that you have ~
used double slashes in all places while writing it.")

(defun furcadia (sname &optional
                         (config *config*) (state *state*)
                         (state-lock *state-lock*))
  "Launches Furcadia for the character with the given shortname."

  (if (stringp (getf *config* :furcadia-path))
      (let* ((login-link (character-login-link sname config state state-lock))
             (furcadia-path (getf *config* :furcadia-path))
             (process (launch-furcadia furcadia-path login-link)))
        (note :info "Furcadia launched for character ~A." sname)
        process)
      (note :error (error-message-furcadia-path))))

(defun initialize ()
  "A high-level function for initializing the launcher."
  (note :info "Loading configuration file.")
  (setf *config* (load-config-file))
  (cond ((null (getf *config* :accounts))
         (note :error "Account credentials are not set in config."))
        ((null (getf *config* :furcadia-path))
         (note :error "Furcadia path is not set in config."))
        (t
         (note :info "All clear - beginning initialization.")
         (login-allf)
         (note :info "All ~D accounts logged in successfully."
               (length (state-cookies *state* *state-lock*)))
         (fetch-all-accountsf)
         (note :info "Data for all ~D accounts fetched successfully."
               (length (state-cookies *state* *state-lock*)))
         (fetch-all-charactersf)
         (note :info "All ~D characters fetched successfully."
               (length (getf *config* :characters)))
         (save-config-file)
         (note :info "Config file saved.")
         (note :info "Initialization complete.
Type (furcadia \"shortname\") in the REPL to launch Furcadia.")
         t)))
