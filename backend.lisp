;;;; backend.lisp

(in-package #:furcadia-launcher)

(defvar *email* nil)
(defvar *password* nil)
(defvar *cookie-jar*)
(defvar *character-name*)
(defvar *login-page*)
(defvar *auth-secret*)
(defvar *fured-page*)
(defvar *login-secret*)
(defvar *character*)
(defvar *login-response*)
(defvar *login-string*)

(defun login-to-furcadia (character-name)
  (let ((*character-name* character-name)
        (*cookie-jar* (make-instance 'cookie-jar))
        (*login-page* nil)
        (*auth-secret* nil)
        (*secret-fured* nil)
        (*fured-page* nil)
        (*login-secret* nil)
        (*character* nil)
        (*login-response* nil)
        (*login-string* nil))
    (assert *email*)
    (assert *password*)
    ;; Step 1
    (setf *login-page* (step-1a))
    (setf *auth-secret* (step-1b))
    (step-1c)
    ;; Step 2
    (setf *fured-page* (step-2a))
    (setf *login-secret* (step-2b))
    (setf *character* (step-2c))
    (setf *login-response* (step-2d))
    (setf *login-string* (step-2e))
    ;; Step 3
    (step-3a)
    t))

(defun input-login-data ()
  (format t "Email:    ")
  (setf *email* (read-line))
  (format t "Password: ")
  (setf *password* (read-line)))

(defun echo (string &optional (stream *standard-output*))
  (format stream "~A~%" string))

(defun step-1a ()
  (echo "STEP 1A: Get the login page.")
  (let ((result (get-login-page)))
    (echo "STEP 1A: success.")
    result))

(defun step-1b ()
  (echo "STEP 1B: Extract the authentication secret.")
  (let ((result (extract-secret *login-page*)))
    (echo "STEP 1B: success.")
    result))

(defun step-1c ()
  (echo "STEP 1C: Log in.")
  (let ((result (post-login *auth-secret*)))
    (echo "STEP 1C: success.")
    result))

(defun step-2a ()
  (echo "STEP 2A: Get the FurEd page.")
  (let ((result (get-fured-page)))
    (echo "STEP 2A: success.")
    result))

(defun step-2b ()
  (echo "STEP 2B: Extract the character login secret.")
  (let ((result (extract-account *fured-page*)))
    (echo "STEP 2B: success.")
    result))

(defun step-2c ()
  (echo "STEP 2C: Fetch the character.")
  (let ((result (get-character *character-name*)))
    (echo "STEP 2C: success.")
    result))

(defun step-2d ()
  (echo "STEP 2D: Upload the character.")
  (let ((result (post-character *character*)))
    (echo "STEP 2D: success.")
    result))

(defun step-2e ()
  (echo "STEP 2E: Extract the login string.")
  (let ((result (get-login-link *character*)))
    (echo "STEP 2E: success.")
    result))

(defun step-3a ()
  (echo "STEP 3A: Launch Furcadia.")
  (let ((result (launch-furcadia *login-string*)))
    (echo "STEP 3A: success.")
    result))





























;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; DONE

(defun extract-secret (response)
  (let* ((start (search "<input type=\"hidden\" name=\"return\"" response))
         (cut-response (subseq response (1+ start)))
         (start (search "<input" cut-response)))
    (subseq cut-response (+ start 27) (+ start 27 32))))

(defun extract-account (&optional (page (get-fured-page)))
  (let* ((begin (search "account.JSON=" page))
         (end (search (string #\Newline) page :start2 begin))
         (subseq (subseq page (+ begin 13) (1- end)))
         (json (decode-json (make-string-input-stream subseq))))
    (setf *secret-fured* (cdr (assoc :session json)))
    json))

(defun get-login-page ()
  (http-request "https://cms.furcadia.com/login"
                :method :get
                :cookie-jar *cookie-jar*))

(defun post-login (secret)
  (http-request
   "https://cms.furcadia.com/index.php?option=com_sphinx&task=user.login"
   :method :post
   :parameters `(("username" . ,*email*)
                 ("password" . ,*password*)
                 (,secret . "1"))
   :cookie-jar *cookie-jar*))

(defun get-fured-page ()
  (http-request "https://cms.furcadia.com/fured/"
                :method :get
                :cookie-jar *cookie-jar*
                :redirect nil))

(defun login ()
  (when (string= *email* "")
    (input-login-data))
  (setf (cookie-jar-cookies *cookie-jar*) nil
        *secret* (extract-secret (get-login-page)))
  (post-login *secret*))

(defun load-character (name)
  (check-type name string)
  (http-request "https://cms.furcadia.com/fured/loadCharacter.php"
                :method :post
                :parameters `(("name" . ,name))
                :cookie-jar *cookie-jar*))

(defun list-characters (&optional (account (extract-account)))
  (mapcar (lambda (x) (cdr (assoc :shortname x)))
          (cdr (assoc :characters account))))

(defun get-all-characters ()
  (mapcar #'get-character (list-characters)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; REQUESTS

(defun post-character (character)
  (let ((headers (construct-save-request character)))
    (http-request "https://cms.furcadia.com/fured/saveCharacter.php"
                  :method :post
                  :additional-headers '(("Origin" . "https://cms.furcadia.com")
                                        ("X-Requested-With" . "XMLHttpRequest"))
                  :accept "application/json, text/javascript, */*; q=0.01"
                  :parameters headers
                  :cookie-jar *cookie-jar*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; DATA

(defvar *save-char-keywords*
  '("colr" "desc" "digo" "wing" "port" "tag" "adesc" "awhsp" "aresp"
    "doresp" "adigo" "awing" "atime" "amaxtime" "acolr" "aport" "uid"))

(defun construct-save-keyword (character keyword)
  (let ((data (or (cdr (assoc keyword character :test #'string-equal)) "")))
    (cons keyword (princ-to-string data))))

(defun construct-save-request (character)
  (nconc (mapcar (lambda (x) (construct-save-keyword character x))
                 *save-char-keywords*)
         (list (cons "tokenRequest" "true")
               (cons "tokenCostume" "-1")
               (cons *secret-fured* "1"))))

(defun get-login-link (character &optional (html (post-character character)))
  (let ((string (make-string-input-stream html)))
    (cdr (assoc :login--url (decode-json string)))))

(defun construct-furcadia-command (link)
  (let ((command (list (cat *furcadia-path* "Furcadia.exe")
                       "-defaultpath" *furcadia-path*
                       "-followurl" link)))
    (if (uiop:os-windows-p)
        command
        (cons "wine" command))))

(defun winepath (path)
  (trim-whitespace
   (with-output-to-string (*standard-output*)
     (uiop:run-program (list "winepath" path)
                       :output t))))

(defun launch-furcadia (login-string)
  (let* ((path (if (uiop:os-windows-p)
                   *furcadia-path*
                   (winepath *furcadia-path*)))
         (command (construct-furcadia-command login-string)))
    (uiop:chdir path)
    (uiop:launch-program command :input nil
                                 :output nil
                                 :error-output nil)))

(defun run-furcadia (name)
  (when (string= *secret* "")
    (login))
  (when (string= *secret-fured* "")
    (extract-account))
  (let* ((character (get-character name))
         (link (get-login-link character)))
    (launch-furcadia link)))
