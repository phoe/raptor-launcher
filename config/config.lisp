;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; config.lisp

(in-package :raptor-launcher/config)

;;; Paths

(defvar *home-path*
  (merge-pathnames ".raptor-launcher/" (user-homedir-pathname))
  "The home directory for Raptor Launcher.")

(defvar *config-path* (merge-pathnames "config/config.lisp" *home-path*)
  "The file for storing Raptor Launcher configuration.")

(defvar *dl-path* (merge-pathnames "dl/" *home-path*)
  "The directory for storing files downloaded by Raptor Launcher.")

;;; Config

(defun config (&rest path)
  "Fetch the configuration value for the given path. Returns two values. The
first is the retrieved value, or NIL if it was not found; the second is true if
the value was found in the configuration."
  (let ((*storage-pathname* *config-path*))
    (apply #'value path)))

(defun default-config (default &rest path)
  "Fetch the configuration value for the given path and return it. If the value
is not found, DEFAULT is set as the value and returned."
  (let ((*storage-pathname* *config-path*))
    (apply #'defaulted-value default path)))

(defmacro econfig (&rest path)
  "Returns the configuration value for the given path. Signals an error if the
value was not found."
  `(multiple-value-bind (value foundp) (config ,@path)
     (if foundp
         value
         (error "Failed to find a configuration value for path ~S." ',path))))

(defun (setf config) (new-value &rest path)
  "Set the configuration value of the given path."
  (let ((*storage-pathname* *config-path*))
    (apply #'(setf value) new-value path)))

(defun remconfig (&rest path)
  "Removes the configuration value of the given path."
  (let ((*storage-pathname* *config-path*))
    (apply #'remvalue path)))

(defmacro with-config-transaction (() &body body)
  "Executes the following form inside a config transaction. This macro is ~
meant to be used whenever multiple configuration forms are set in one body of ~
code to avoid unnecessary disk writes."
  `(with-transaction () ,@body))

(defvar *config-alist-collection* '())

(defun config-alist (&optional config)
  "Converts the provided configuration into an alist, where keys are lists ~
denoting the depth, and values are respective configuration values."
  (let ((*config-alist-collection* '())
        (config (or config (config))))
    (%config-alist config '())))

(defun %config-alist (config path)
  (loop for key being the hash-keys of config
        for value = (gethash key config)
        for current-path = (append path (list key))
        if (hash-table-p value)
          do (%config-alist value current-path)
        else
          do (push (cons current-path value) *config-alist-collection*)
        finally (return *config-alist-collection*)))
