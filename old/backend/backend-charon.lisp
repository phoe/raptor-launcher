;;;; backend-charon.lisp

(in-package :furcadia-launcher)

(defvar *charon-secret*
  (error "Fill the *charon-secret* in before you compile this file.")
  "The Charon API secret.")

(defvar *http-charon-clogin* "https://charon.furcadia.com/accounts/clogin.php"
  "The Charon account login endpoint.")

(defun http-charon-load-account (email password)
  "Performs the HTTP request to Charon account login with the provided email
and password."
  (let ((version (cat "Raptor Launcher " *version*)))
    (http-request *http-charon-clogin*
                  :method :post
                  :parameters `(("k" . ,*charon-secret*)
                                ("v" . ,version)
                                ("u" . ,email)
                                ("p" . ,password)))))

(defun charon-load-account (email password)
  "Logs the account in using the provided email and password, returning
a parsed XML response containing account and character data."
  (let ((response (http-charon-load-account email password)))
    (assert (vectorp response))
    (let ((parsed-response
            (cxml:parse-octets response (cxml-xmls:make-xmls-builder))))
      (keywordize-cars parsed-response))))

(defmacro charon-load-accountf (email password)
  "Logs the account in as with CHARON-LOAD-ACCOUNT, then calls SETF
STATE-CHARON-INFO on target email to set the retrieved data."
  `(setf (state-charon-info *state* *state-lock* ,email)
         (charon-preparse (charon-load-account ,email ,password))))

(defun charon-preparse (list)
  "Preparses the whole Charon response."
  (let ((character-list (cddr (caddr list))))
    (loop for list in character-list
          for i from 0
          do (setf (nth i character-list) (charon-preparse-character list)))
    character-list))

(defun charon-preparse-character (list)
  "Preparses a single :CHAR entry from a Charon response."
  (let* ((list (cdr list))
         (desc-list (cadr list)))
    (nconc (first list) (list (second list)))
    (setf (cdr list) (cddr list)
          (cdr desc-list) (cddr desc-list))
    list))
