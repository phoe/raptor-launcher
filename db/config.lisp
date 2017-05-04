;;;; config.lisp

(in-package :furcadia-launcher)

;; TODO make this race-condition-free, implement a lock and custom accessor
(defvar *config* nil)
(defvar *config-path* "~/.furcadia-launcher/config.lisp")

(defvar *sample-config*
  '(:furcadia-path "C:\\Furcadia\\"
    :accounts (("foo@bar.com" "FooBar1234")
               ("frob@mem.org" "Frobnicate!@#4"))))

(defun load-config-file ()
  "Reads and returns the configuration file from disk.
Note that this function does not set the file to the *CONFIG* var - you need to
do it manually."
  (ensure-directories-exist *config-path*)
  (prog1 (with-open-file (s *config-path*
                            :direction :input
                            :if-does-not-exist :create)
           (read s nil nil))
    (note :info "Configuration file loaded.")))

(defun save-config-file (&optional (config *config*))
  "Saves the current config to disk. If no config is provided, the value of
*CONFIG* is used."
  (ensure-directories-exist *config-path*)
  (with-open-file (s *config-path*
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists :supersede)
    (print config s)
    (terpri s)
    (note :info "Configuration saved to disk.")
    t))
