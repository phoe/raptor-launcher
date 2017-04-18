;;;; backend-new.lisp

(in-package :furcadia-launcher)

(defvar *http-login-page*
  "https://cms.furcadia.com/login")

(defun http-get-login-page (cookie-jar)
  (let* ((page (http-request *http-login-page*
                             :cookie-jar cookie-jar)))
    page))

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
    page))

(defun do-login (email password)
  (let* ((cookie-jar (make-instance 'cookie-jar))
         (login-page (http-get-login-page cookie-jar))
         (login-secret (extract-login-page-secret login-page)))
    (http-post-login email password login-secret cookie-jar)
    cookie-jar))
