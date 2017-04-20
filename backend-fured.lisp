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

(defun extract-fured-account-info (fured-page)
  "Extracts the account JSON from the FurEd page."
  (let* ((begin (search "account.JSON=" fured-page))
         (end (search (string #\Newline) fured-page :start2 begin))
         (subseq (subseq fured-page (+ begin 13) (1- end)))
         (json (decode-json (make-string-input-stream subseq))))
    (assert (assoc :main json))
    json))

(defun extract-fured-page-secret (account-json)
  "Extracts the FurEd secret from the account JSON."
  (let* ((result (cdr (assoc :session account-json))))
    (assert (hexadecimal-string-p result))
    result))

(defvar *http-load-character*
  "https://cms.furcadia.com/fured/loadCharacter.php"
  "Address of the loadCharacter furEd page.")

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
