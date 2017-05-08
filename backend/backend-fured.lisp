;;;; backend-fured.lisp

(in-package :furcadia-launcher)

(defvar *http-fured-page*
  "https://cms.furcadia.com/fured/"
  "Address of the main FurEd page.")

(defun http-get-fured-page (cookie-jar)
  "Loads the FurEd page, using the provided cookie jar with login cookies."
  (http-request *http-fured-page*
                :cookie-jar cookie-jar))

(defun extract-fured-account-json (fured-page)
  "Extracts the account JSON from the FurEd page."
  (let* ((begin (search "account.JSON=" fured-page))
         (end (search (string #\Newline) fured-page :start2 begin))
         (subseq (subseq fured-page (+ begin 13) (1- end)))
         (json (decode-json (make-string-input-stream subseq))))
    (assert (assoc :main json))
    (note :info "Extracted account information for ~A."
          (cdr (assoc :email json)))
    json))

(defvar *http-load-character*
  "https://cms.furcadia.com/fured/loadCharacter.php"
  "Address of the loadCharacter FurEd page.")

(defun http-load-character (sname cookie-jar)
  "Loads the character JSON with the provided shortname, using the provided
cookie jar."
  (http-request *http-load-character*
                :method :post
                :parameters `(("name" . ,sname))
                :cookie-jar cookie-jar))

(defun decode-character (load-character-page)
  "Decodes and returns the character JSON using the provided load character HTTP
request."
  (let* ((stream (make-string-input-stream load-character-page))
         (json (decode-json stream)))
    (assert (assoc :name json))
    (note :info "Loaded character ~A." (cdr (assoc :snam json)))
    json))

(defun list-characters (account-json)
  "Extracts the list of character shortnames from the account JSON."
  (mapcar (lambda (x) (cdr (assoc :shortname x)))
          (cdr (assoc :characters account-json))))

(defun character-last-login-list (&optional (state *state*)
                                    (state-lock *state-lock*))
  "Provided a state and a state lock, returns a list of characters from all
accounts, where each element is an alist containing a character's name,
shortname and login date Unix timestamp."
  (let* ((accounts (with-lock-held (state-lock) (gethash :accounts state)))
         (list (mappend (rcurry #'assoc-value :characters) accounts)))
    (flet ((fn (x) (substitute #\Space #\| x)))
      (loop for alist in list
            collect (loop for (key . value) in alist
                          when (eq key :name)
                            collect (cons key (fn value))
                          else collect (cons key value))))))

(defun character-last-login (sname &optional (state *state*)
                                     (state-lock *state-lock*))
  "Provided a shortname, a state and a state lock, returns a string containing
the last login date of the character by that shortname in that state, in the
format \"YYYY-MM-DD HH:MM:SS\"."
  (let* ((list (state-last-logins state state-lock))
         (cell (find sname list :key (rcurry #'assoc-value :shortname)
                                :test #'string=))
         (unixtime (assoc-value cell :login-date)))
    (unix-time-to-datestring unixtime)))

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

(defvar *save-char-basic-keywords*
  '("desc" "colr" "port" "uid" "digo")
  "Keywords to take into account when generating the save-character JSON
purely for the purpose of fetching the download link.")

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
