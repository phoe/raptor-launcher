;;;; backend-fured.lisp

(in-package :furcadia-launcher)

(defvar *http-fured-page*
  "https://cms.furcadia.com/fured/"
  "Address of the main FurEd page.")

(defun http-get-fured-page (cookie-jar)
  "Loads the FurEd page, using the provided cookie jar.
Cookies from login are required to load the page."
  (http-request *http-fured-page*
                :cookie-jar cookie-jar))

(defun extract-fured-account-json (fured-page)
  "Extracts the account JSON from the FurEd page."
  (let* ((begin (search "account.JSON=" fured-page))
         (end (search (string #\Newline) fured-page :start2 begin))
         (subseq (subseq fured-page (+ begin 13) (1- end)))
         (json (decode-json (make-string-input-stream subseq))))
    (assert (assoc :main json))
    json))

(defvar *http-load-character*
  "https://cms.furcadia.com/fured/loadCharacter.php"
  "Address of the loadCharacter FurEd page.")

(defun http-load-character (name cookie-jar)
  "Loads the character JSON with the provided shortname, using the provided
cookie jar."
  (let* ((data (http-request *http-load-character*
                             :method :post
                             :parameters `(("name" . ,name))
                             :cookie-jar cookie-jar))
         (stream (make-string-input-stream data))
         (json (decode-json stream)))
    (assert (assoc :name json))
    json))

(defun list-characters (account-json)
  "Extracts the list of character shortnames from the account JSON."
  (mapcar (lambda (x) (cdr (assoc :shortname x)))
          (cdr (assoc :characters account-json))))

(defun get-all-characters (account-json cookie-jar)
  "Loads all character JSONs associated to the provided account JSON,
using the provided cookie jar."
  (mapcar (lambda (x) (http-load-character x cookie-jar))
          (list-characters account-json)))

(defun extract-fured-page-secret (account-json)
  "Extracts the FurEd secret from the account JSON."
  (let* ((result (cdr (assoc :session account-json))))
    (assert (hexadecimal-string-p result))
    result))

(defvar *save-char-keywords*
  '("colr" "desc" "digo" "wing" "port" "tag" "adesc" "awhsp" "aresp"
    "doresp" "adigo" "awing" "atime" "amaxtime" "acolr" "aport" "uid")
  "Keywords to take into account when generating the save-chracter JSON.")

(defun construct-save-keyword (character-json keyword)
  "Constructs a single key-value pair of the JSON to be saved for the provided
character JSON and keyword."
  (let ((data (or (cdr (assoc keyword character-json :test #'string-equal)) "")))
    (cons keyword (princ-to-string data))))

(defun construct-save-request (character-json fured-secret)
  "Constructs a full character-save JSON for the provided character JSON and
FurEd secret."
  (nconc (mapcar (lambda (x) (construct-save-keyword character-json x))
                 *save-char-keywords*)
         (list (cons "tokenRequest" "true")
               (cons "tokenCostume" "-1")
               (cons fured-secret "1"))))

(defvar *http-save-character*
  "https://cms.furcadia.com/fured/saveCharacter.php"
  "Address of the saveCharacter FurEd page.")

(defun http-save-character (character-json cookie-jar fured-secret)
  "Saves the provided character JSON, using the provided cookie jar and
FurEd secret. Returns the client login JSON."
  (let ((headers (construct-save-request character-json fured-secret)))
    (http-request *http-save-character*
                  :method :post
                  :parameters headers
                  :cookie-jar cookie-jar)))

(defun extract-login-link (save-character-json)
  "Extracts the client login link from the provided client login JSON."
  (let* ((string (make-string-input-stream save-character-json))
         (result (cdr (assoc :login--url (decode-json string)))))
    (assert (stringp result))
    result))
