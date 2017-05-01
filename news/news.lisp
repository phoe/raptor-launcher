;;;; news.lisp

(in-package :furcadia-launcher)

(defvar *news-download-dir* "~/.furcadia-launcher/news/")

(defun http-get-news-furcadia ()
  (split-news (replace-all (flexi-streams:octets-to-string
                            (http-request "http://news.furcadia.com/current"
                                          :external-format-out :utf-8))
                           "#LF#" "")))

(defun http-get-news-launcher ()
  (split-news (http-request "http://raptorlauncher.github.io/news.txt"
                            :external-format-out :utf-8)))

(defun split-news (news)
  (flet ((cut-newsentry (x) (setf (car x) (subseq (car x) 10)) x)
         (skip-second (x) (setf (cdr x) (cddr x)) x)
         (add-filename (x) (cons (url-filename (sixth x)) x))
         (add-date (x) (cons (multiply-date (cadr x)) x)))
    (let* ((data (split-sequence #\Newline news :remove-empty-subseqs t))
           (news (nthcdr 8 data))
           (lastcons (nthcdr 7 data))
           (*separator* #\Tab)
           (fn (compose #'add-date
                        #'add-filename
                        #'cut-newsentry
                        #'skip-second
                        #'read-csv-line
                        #'make-string-input-stream)))
      (setf (cdr lastcons) nil)
      (values (mapcar fn news) (nth 7 data)))))

;; (defun get-all-news ()
;;   (multiple-value-bind (furcadia-news furcadia-date) (http-get-news-furcadia)
;;     (multiple-value-bind (launcher-news launcher-date) (http-get-news-launcher)
;;       )))

(defun get-news ()
  (split-news (http-get-news)))

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
