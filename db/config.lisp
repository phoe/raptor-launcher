;;;; config.lisp

(in-package :furcadia-launcher)

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
  (with-open-file (s *config-path*
                     :direction :input
                     :if-does-not-exist :create)
    (read s nil nil)))

(defun write-dummy-config-file ()
  "Writes a dummy config file in the event there is none."
  (ensure-directories-exist *config-path*)
  (with-open-file (s *config-path*
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists :overwrite)
    (print *sample-config* s)
    (terpri s)
    t))

(defun save-config-file (&optional (config *config*))
  "Saves the current config to disk. If no config is provided, the value of
*CONFIG* is used."
  (ensure-directories-exist *config-path*)
  (with-open-file (s *config-path*
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists :overwrite)
    (print config s)
    (terpri s)
    t))
