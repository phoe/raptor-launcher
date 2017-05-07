;;;; news.lisp

(in-package :furcadia-launcher)

(defvar *news-download-dir* "~/.furcadia-launcher/news/"
  "The directory where news images are downloaded and stored.")

(defun http-furcadia-news ()
  (split-news (replace-all (flexi-streams:octets-to-string
                            (http-request "http://news.furcadia.com/current"
                                          :external-format-out :utf-8))
                           "#LF#" "")))

(defun http-launcher-news ()
  (split-news (http-request "http://raptorlauncher.github.io/news.txt"
                            :external-format-out :utf-8)))

(defun split-news (news)
  (flet ((cut-newsentry (x) (setf (car x) (subseq (car x) 10)) x)
         (skip-second (x) (setf (cdr x) (cddr x)) x)
         (add-filename (x) (cons (url-filename (sixth x)) x))
         (add-date (x) (cons (multiply-date (cadr x)) x)))
    (let* ((data (split-sequence #\Newline news :remove-empty-subseqs t))
           (news (nthcdr 8 data))
           (*separator* #\Tab)
           (fn (compose #'add-date
                        #'add-filename
                        #'cut-newsentry
                        #'skip-second
                        #'read-csv-line
                        #'make-string-input-stream)))
      (values (mapcar fn news) (nth 7 data)))))

(defun get-all-news ()
  (handler-case
      (multiple-value-bind (furcadia-news furcadia-date) (http-furcadia-news)
        (note :info "Furcadia news downloaded successfully.")
        (multiple-value-bind (launcher-news launcher-date) (http-launcher-news)
          (note :info "Raptor Launcher news downloaded successfully.")
          (let ((urls-1 (mapcar (curry #'nth 7) furcadia-news))
                (urls-2 (mapcar (curry #'nth 7) launcher-news))
                (news (sort (nconc furcadia-news launcher-news) #'> :key #'car))
                (updatedp (update-news-date furcadia-date launcher-date)))
            (mapc #'download-news-image urls-1)
            (mapc #'download-news-image urls-2)
            (when updatedp
              (note :info "News contain new items."))
            (values news updatedp))))
    (error (e)
      (note :error "There was an error fetching news: ~A. ~
Check your network connection." e)
      (values nil nil))))

(defvar *fetched-news* nil)
(defvar *fetched-updatedp* nil)

(defun get-all-newsf ()
  (multiple-value-bind (news updatedp) (get-all-news)
    (setf *fetched-news* news
          *fetched-updatedp* updatedp)
    (values news updatedp)))

(defun update-news-date (furcadia-date launcher-date)
  (let* ((old-furcadia-date (getf *config* :furcadia-date))
         (old-launcher-date (getf *config* :launcher-date))
         (furcadia-diff (string/= furcadia-date old-furcadia-date))
         (launcher-diff (string/= launcher-date old-launcher-date)))
    (when furcadia-diff
      (setf (getf *config* :furcadia-date) furcadia-date))
    (when launcher-diff
      (setf (getf *config* :launcher-date) launcher-date))
    (or furcadia-diff launcher-diff)))

(defun multiply-date (datestring)
  (let ((date (date-parser:parse-date datestring)))
    (+ (first date)
       (* 100 (second date))
       (* 10000 (third date)))))

(defun url-filename (url)
  (let* ((position (1+ (position #\/ url :from-end t)))
         (filename (subseq url position)))
    filename))

(defun news-image-filename-pathname (filename)
  (merge-pathnames filename *news-download-dir*))

(defun news-image-present-p (url)
  (let* ((filename (url-filename url))
         (filepath (merge-pathnames filename *news-download-dir*)))
    (probe-file filepath)))

(defun download-news-image (url)
  (ensure-directories-exist *news-download-dir*)
  (let* ((filename (url-filename url))
         (filepath (merge-pathnames filename *news-download-dir*)))
    (handler-case
        (with-open-file (file filepath
                              :direction :output
                              :if-does-not-exist :create
                              :if-exists nil
                              :element-type '(unsigned-byte 8))
          (when file
            (note :info "Downloading news image ~A." filename)
            (let ((input (drakma:http-request url :want-stream t)))
              (unwind-protect
                   (loop for x = (read-byte input nil nil)
                         while x do (write-byte x file))
                (close input))))
          filepath)
      (error (e)
        (note :error "Failed to download news image ~A: ~A" filename e)))))
