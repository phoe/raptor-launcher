;;;; config.lisp

(in-package :furcadia-launcher)

;;;;;; CONFIGURATION - EDIT THESE

;; (defvar *furcadia-path* "C:\\Furcadia\\") ;; remember double-slashes here

(defvar *cookie-jar* (make-instance 'cookie-jar))
(defparameter *secret* "")
(defparameter *secret-fured* "")

(defvar *config* nil)
(defvar *config-path* "~/.furcadia-launcher/config.lisp")

(defvar *sample-config*
  '(:accounts (("foo@bar.com" "FooBar1234")
               ("frob@mem.org" "Frobnicate!@#4"))))

(defun load-config-file ()
  (ensure-directories-exist *config-path*)
  (with-open-file (s *config-path*
                     :direction :input
                     :if-does-not-exist :create)
    (read s nil nil)))

(defun save-config-file (&optional (config *config*))
  (ensure-directories-exist *config-path*)
  (with-open-file (s *config-path*
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists :overwrite)
    (print config s)
    (terpri s)))

(defvar *http-login-page*
  "https://cms.furcadia.com/login")

(defun http-get-login-page ()
  (let* ((cookie-jar (make-instance 'cookie-jar))
         (page (http-request *http-login-page*
                             :method :get
                             :cookie-jar cookie-jar)))
    (values page cookie-jar)))

(defun extract-login-page-secret (page)
  (let* ((start (search "<input type=\"hidden\" name=\"return\"" page))
         (cut-page (subseq page (1+ start)))
         (start2 (search "<input" cut-page))
         (result (subseq cut-page (+ start2 27) (+ start2 27 32))))
    (assert (hexadecimal-string-p result))
    result))

(defvar *http-login-post*
  "https://cms.furcadia.com/index.php?option=com_sphinx&task=user.login")

(defun http-post-login (email password login-secret cookie-jar)
  (let ((page (http-request *http-login-post*
                            :method :post
                            :parameters `(("username" . ,email)
                                          ("password" . ,password)
                                          (,login-secret . "1"))
                            :cookie-jar cookie-jar)))
    (assert (search "Logout" page))
    (values page cookie-jar)))
