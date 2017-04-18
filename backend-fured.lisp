;;;; backend-fured.lisp

(defvar *http-fured-page*
  "https://cms.furcadia.com/fured/")

(defun http-get-fured-page (cookie-jar)
  (http-request *http-fured-page*
                :cookie-jar cookie-jar))

(defun extract-fured-account-info (page)
  (let* ((begin (search "account.JSON=" page))
         (end (search (string #\Newline) page :start2 begin))
         (subseq (subseq page (+ begin 13) (1- end)))
         (json (decode-json (make-string-input-stream subseq))))
    (assert (assoc :main json))
    json))

(defun extract-fured-page-secret (page)
  (let* ((result (extract-fured-account-info page)))
    (assert (hexadecimal-string-p result))
    result))

(defvar *http-load-character*
  "https://cms.furcadia.com/fured/loadCharacter.php")

(defun http-load-character (name cookie-jar)
  (let* ((data (http-request *http-load-character*
                             :method :post
                             :parameters `(("name" . ,name))
                             :cookie-jar cookie-jar))
         (stream (make-string-input-stream data))
         (json (decode-json stream)))
    (assert (assoc :name json))
    json))

(defun list-characters (account-info)
  (mapcar (lambda (x) (cdr (assoc :shortname x)))
          (cdr (assoc :characters account-info))))

(defun get-all-characters (account-info cookie-jar)
  (mapcar (lambda (x) (http-load-character x cookie-jar))
          (list-characters account-info)))
