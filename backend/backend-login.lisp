;;;; backend-login.lisp

(in-package :furcadia-launcher)

(defvar *http-login-page*
  "https://cms.furcadia.com/login"
  "Address of the Furcadia CMS login page.")

(defun http-get-login-page (cookie-jar)
  "Loads the Furcadia CMS login page."
  (http-request *http-login-page*
                :cookie-jar cookie-jar))

(defun extract-login-page-secret (page)
  "Extracts the login secret from the login page."
  (let* ((start (search "<input type=\"hidden\" name=\"return\"" page))
         (cut-page (subseq page (1+ start)))
         (start2 (search "<input" cut-page))
         (result (subseq cut-page (+ start2 27) (+ start2 27 32))))
    (assert (hexadecimal-string-p result))
    result))

(defvar *http-login-post*
  "https://cms.furcadia.com/index.php?option=com_sphinx&task=user.login"
  "Address of the POST login page.")

(defun http-post-login-parameters (email password login-secret)
  "Formats the email, password and login secret in a way suitable for Drakma."
  `(("username" . ,email)
    ("password" . ,password)
    (,login-secret . "1")))

(defun http-post-login (email password login-secret cookie-jar)
  "Makes a login POST request, using the provided email, password, login secret
and cookie jar. The cookie jar is modified to hold the login cookies."
  (let* ((parameters (http-post-login-parameters email password login-secret))
         (page (http-request *http-login-post*
                             :method :post
                             :parameters parameters
                             :cookie-jar cookie-jar)))
    (assert (search "Logout" page))
    (note :info "Successfully logged in as ~A." email)
    page))

(defun do-login (email password)
  "Performs a full login with the provided email and password, returning the
cookie jar with associated login cookies."
  (note :info "Attempting to log as ~A." email)
  (let* ((cookie-jar (make-instance 'cookie-jar))
         (login-page (http-get-login-page cookie-jar))
         (login-secret (extract-login-page-secret login-page)))
    (http-post-login email password login-secret cookie-jar)
    cookie-jar))
