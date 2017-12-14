;;;; backend-fured.lisp

(in-package :furcadia-launcher)

(defun extract-fured-page-secret (account-json)
  "Extracts the FurEd secret from the account JSON."
  (let* ((result (cdr (assoc :session account-json))))
    (assert (hexadecimal-string-p result))
    result))

(defvar *save-char-keywords*
  '("colr" "desc" "digo" "wing" "port" "tag" "adesc" "awhsp" "aresp"
    "doresp" "adigo" "awing" "atime" "amaxtime" "acolr" "aport" "uid")
  "Keywords to take into account when generating the save-chracter JSON
for the purpose of saving the character.")

;; (defvar *save-char-basic-keywords*
;;   '("desc" "colr" "port" "uid" "digo")
;;   "Keywords to take into account when generating the save-character JSON
;; purely for the purpose of fetching the download link.")

(defun construct-save-keyword (character-json keyword)
  "Constructs a single key-value pair of the JSON to be saved for the provided
character JSON and keyword."
  (let* ((value (assoc-value character-json keyword :test #'string-equal))
         (data (or value "")))
    (cons keyword (princ-to-string data))))

(defun construct-save-request (character-json fured-secret &optional basic-only)
  "Constructs a full character-save JSON for the provided character JSON and
FurEd secret."
  (nconc (mapcar (lambda (x) (construct-save-keyword character-json x))
                 (if basic-only
                     *save-char-basic-keywords*
                     *save-char-keywords*))
         (list (cons "tokenRequest" "true")
               (cons "tokenCostume" "-1")
               (cons fured-secret "1"))))

(defvar *http-save-character*
  "https://cms.furcadia.com/fured/saveCharacter.php"
  "Address of the saveCharacter FurEd page.")

(defun http-save-character (character-json cookie-jar fured-secret
                            &optional basic-only)
  "Saves the provided character JSON, using the provided cookie jar and
FurEd secret. Returns the client login JSON."
  (let* ((headers (construct-save-request character-json fured-secret
                                          basic-only))
         (result (http-request *http-save-character*
                               :method :post
                               :parameters headers
                               :cookie-jar cookie-jar))
         (json (decode-json (make-string-input-stream result))))
    (assert (string= (cdr (assoc :state json)) "success"))
    (note :info "Saved character ~A." (cdr (assoc :snam character-json)))
    json))

(defun extract-login-link (save-character-json)
  "Extracts the client login link from the provided client login JSON."
  (let* ((result (cdr (assoc :login--url save-character-json))))
    (assert (stringp result))
    result))
