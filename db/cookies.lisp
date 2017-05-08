;;;; cookies.lisp

(in-package :furcadia-launcher)

(defun serialize-cookie-jars (list)
  "Provided an alist of emails and cookie jars, returns an alist of emails and
serialized cookie jars."
  (loop for (email . cookie-jar) in list
        collect (cons email (serialize-cookie-jar cookie-jar))))

(defun serialize-cookie-jar (cookie-jar)
  "Provided a cookie jar, returns a serialized list of plists, suitable for
using in MAKE-INSTANCE COOKIE."
  (loop for cookie in (cookie-jar-cookies cookie-jar)
        collect `(:name ,(cookie-name cookie)
                  :domain ,(cookie-domain cookie)
                  :value ,(cookie-value cookie)
                  :path ,(cookie-path cookie)
                  :expires ,(cookie-expires cookie)
                  :securep ,(cookie-securep cookie)
                  :http-only-p ,(cookie-http-only-p cookie))))

(defun deserialize-cookie-jar (list)
  "Provided a serialized cookie jar, returns a cookie jar objects in which
cookies are COOKIE= to the original cookies before the serialization."
  (let ((cookies (mapcar (curry #'apply #'make-instance 'cookie) list)))
    (make-instance 'cookie-jar :cookies cookies)))

(defun deserialize-cookie-jars (list)
  "Provided an alist of emails and serialized cookie jars, returns an alist of
emails and serialized cookie jars, suitable for calling SETF STATE-COOKIES on."
  (loop for (email . serialized-jar) in list
        collect (cons email (deserialize-cookie-jar serialized-jar))))
