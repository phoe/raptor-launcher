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

;;; Config - test TODO move out of code

(defun test ()
  (uiop:with-temporary-file (:pathname pathname)
    (let ((*storage-pathname* pathname))
      (assert (equal (multiple-value-list (config :foo)) '(nil nil)))
      (setf (config :foo) :bar)
      (assert (equal (multiple-value-list (config :foo)) '(:bar t)))
      (remconfig :foo)
      (assert (equal (multiple-value-list (config :foo)) '(nil nil))))))
