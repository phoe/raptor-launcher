;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; framework.lisp

(in-package :raptor-launcher/raptor-picker)
(in-readtable :qtools)

(defun make-picker-petri-net ()
  (threaded-petri-net ()
    (credentials -> #'login -> cookie-jars
                 -> #'dl-account -> accounts (accounts-furres *))
    (accounts-furres -> #'dl-furre
                     -> furres furres-images
                     (furres-costumes *)
                     (furres-portraits *)
                     (furres-specitags *))
    (furres-costumes -> #'dl-costume -> costumes)
    (furres-portraits -> #'dl-portrait -> portraits)
    (furres-specitags -> #'dl-specitag -> specitags)
    (furres-images -> #'dl-image-list -> (image-metadata *))
    (image-metadata -> #'dl-image -> images)))

(defun test-petri ()
  (let ((petri (make-picker-petri-net)))
    (bag-insert (bag-of petri 'credentials)
                `(,*test-email* ,*test-password*))
    petri))

;;; Utility

(defun raptor-picker ()
  (when *main-window*
    (find (find-class 'raptor-picker) (loaded-modules *main-window*)
          :key #'class-of)))

(define-condition raptor-picker-error (error simple-condition) ())

(defun raptor-picker-error (message &rest args)
  (error 'raptor-picker-error :format-control message :format-arguments args))

(defmacro with-log-on-error ((condition-var message &rest args) &body body)
  `(handler-bind
       ((error (lambda (,condition-var) (note t :error ,message ,@args))))
     ,@body))

(defvar *test-email*)
(defvar *test-password*)

;;; LOGIN

(defun login (input output)
  (destructuring-bind (email password) (pop (gethash 'credentials input))
    (note t :debug "Attempting to log in as ~A." email)
    (with-log-on-error (e "Failed to log in as ~A: ~A" email e)
      (let ((cookie-jar (cl-furcadia/ws:login email password)))
        (unless cookie-jar
          (raptor-picker-error "Invalid credentials for email ~A." email))
        (note t :debug "Successfully logged in as ~A." email)
        (let ((data (list email cookie-jar)))
          (push data (gethash 'cookie-jars output)))
        (when-let ((picker (raptor-picker)))
          (signal! picker (login-completed string) email))))))

;; (defun test-login ()
;;   (let ((output (make-hash-table)))
;;     (login (plist-hash-table `(credentials ((,*test-email* ,*test-password*))))
;;            output)
;;     (hash-table-plist output)))

;;; DL-ACCOUNT

(defun dl-account (input output)
  (destructuring-bind (email cookie-jar) (pop (gethash 'cookie-jars input))
    (note t :debug "Fetching account ~A." email)
    (with-log-on-error (e "Failed to fetch account ~A: ~A" email e)
      (multiple-value-bind (account snames last-logins)
          (cl-furcadia/ws:fetch-account cookie-jar)
        (note t :debug "Successfully fetched account ~A." email)
        (setf (cl-furcadia:cookie-jar-of account) cookie-jar)
        (push account (gethash 'accounts output))
        (loop for sname in snames
              for last-login in last-logins
              for data = (list account sname last-login)
              do (push data (gethash 'accounts-furres output)))
        (when-let ((picker (raptor-picker)))
          (signal! picker (account-downloaded string int)
                   email (length snames)))))))

;; (defun test-dl-account ()
;;   (let ((output (make-hash-table)))
;;     (dl-account (plist-hash-table (test-login)) output)
;;     (hash-table-plist output)))

;;; DL-FURRE

(defun dl-furre (input output)
  (destructuring-bind (account sname last-login)
      (pop (gethash 'accounts-furres input))
    (note t :debug "Fetching furre ~A." sname)
    (with-log-on-error (e "Failed to fetch furre ~A: ~A" sname e)
      (let ((cookie-jar (cl-furcadia:cookie-jar-of account)))
        (multiple-value-bind (furre unknowns)
            (cl-furcadia/ws:fetch-furre sname cookie-jar)
          (if unknowns
              (note t :warn "Successfully fetched furre ~A, but unknown ~
keywords were encountered (possible bug?): ~A" unknowns)
              (note t :debug "Successfully fetched furre ~A." sname))
          (setf (cl-furcadia:last-login furre) last-login
                (cl-furcadia:account furre) account)
          (push furre (gethash 'furres output))
          (push furre (gethash 'furres-images output))
          (dolist (costume (cl-furcadia:costumes furre))
            (push (list furre costume) (gethash 'furres-costumes output)))
          (dolist (portrait (cl-furcadia:portraits furre))
            (push (list furre portrait) (gethash 'furres-portraits output)))
          (dolist (specitag (cl-furcadia:specitags furre))
            (push (list furre specitag) (gethash 'furres-specitags output)))
          (when-let ((picker (raptor-picker)))
            (let ((nspecitags (length (cl-furcadia:specitags furre)))
                  (nportraits (length (cl-furcadia:portraits furre)))
                  (ncostumes (length (cl-furcadia:costumes furre))))
              (signal! picker (furre-downloaded string int int int)
                       sname nspecitags nportraits ncostumes))))))))

;; (defun test-dl-furre ()
;;   (let* ((input (plist-hash-table (test-dl-account)))
;;          (accounts (gethash 'accounts-furres input)))
;;     (loop for account in accounts
;;           for new-input = (make-hash-table)
;;           for new-output = (make-hash-table)
;;           do (push account (gethash 'accounts-furres new-input))
;;              (dl-furre new-input new-output)
;;           append (gethash 'furres new-output))))

;;; DL-COSTUME

(defun dl-costume (input output)
  (destructuring-bind (furre costume) (pop (gethash 'furres-costumes input))
    (let ((sname (cl-furcadia:shortname furre))
          (cookie-jar (cl-furcadia:cookie-jar-of (cl-furcadia:account furre))))
      (with-log-on-error (e "Failed to fetch costume ~A for furre ~A: ~A"
                            costume sname e)
        (typecase costume
          (cl-furcadia::costume
           (note t :debug "Successfully fetched default costume for furre ~A."
                 sname)
           (push costume (gethash 'costumes output)))
          (list
           (destructuring-bind (n cid costume-name) costume
             (declare (ignore n))
             (note t :debug "Fetching costume ~A (~A) for furre ~A."
                   cid costume-name sname)
             (multiple-value-bind (dl-costume unknowns)
                 (cl-furcadia/ws:fetch-costume cid cookie-jar)
               (if unknowns
                   (note t :warn "Successfully fetched costume ~A (~A) for ~
furre ~A, but unknown keywords were encountered (possible bug?): ~A"
                         cid costume-name sname unknowns)
                   (note t :debug "Successfully fetched costume ~A (~A) for ~
furre ~A." cid costume-name sname))
               (setf (cl-furcadia:furre dl-costume) furre)
               (push dl-costume (gethash 'costumes output))
               (when-let ((picker (raptor-picker)))
                 (signal! picker (costume-downloaded string int)
                          sname cid))))))))))

;;; DL-PORTRAIT

(defun dl-portrait (input output)
  (destructuring-bind (furre pid) (pop (gethash 'furres-portraits input))
    (let ((sname (cl-furcadia:shortname furre))
          (cookie-jar (cl-furcadia:cookie-jar-of (cl-furcadia:account furre))))
      (with-log-on-error (e "Failed to fetch portrait ~A for furre ~A: ~A"
                            pid sname e)
        (note t :debug "Fetching portrait ~A for furre ~A." pid sname)
        (let ((portrait (cl-furcadia/ws:fetch-portrait pid cookie-jar)))
          (note t :debug "Successfully fetched portrait ~A for furre ~A."
                pid sname)
          (setf (cl-furcadia:furre portrait) furre)
          (push portrait (gethash 'portraits output))
          (when-let ((picker (raptor-picker)))
            (signal! picker (portrait-downloaded string int)
                     sname pid)))))))

;;; DL-SPECITAG

(defun dl-specitag (input output)
  (destructuring-bind (furre sid) (pop (gethash 'furres-specitags input))
    (let ((sname (cl-furcadia:shortname furre)))
      (with-log-on-error (e "Failed to fetch specitag ~A for furre ~A: ~A"
                            sid sname e)
        (note t :debug "Fetching specitag ~A for furre ~A." sid sname)
        (let ((specitag (cl-furcadia/ws:fetch-specitag sid)))
          (note t :debug "Successfully fetched specitag ~A for furre ~A."
                sid sname)
          (setf (cl-furcadia:furre specitag) furre)
          (push specitag (gethash 'specitags output))
          (when-let ((picker (raptor-picker)))
            (signal! picker (specitag-downloaded string int)
                     sname sid)))))))

;;; DL-IMAGE-LIST

(defun dl-image-list (input output)
  (let* ((furre (pop (gethash 'furres-images input)))
         (sname (cl-furcadia:shortname furre)))
    (with-log-on-error (e "Failed to fetch image metadata for furre ~A: ~A"
                          sname e)
      (note t :debug "Fetching image metadata for furre ~A." sname)
      (let ((images (cl-furcadia/ws:fetch-image-list sname)))
        (note t :debug
              "Successfully fetched metadata for ~D images for furre ~A."
              (length images) sname)
        (dolist (image images)
          (setf (cl-furcadia:furre image) furre)
          (push image (gethash 'image-metadata output)))
        (when-let ((picker (raptor-picker)))
          (signal! picker (image-list-downloaded string int)
                   sname (length images)))))))

;;; DL-IMAGE

(defun dl-image (input output)
  (let* ((image (pop (gethash 'image-metadata input)))
         (sname (cl-furcadia:shortname (cl-furcadia:furre image))))
    (with-log-on-error (e "Failed to fetch image for furre ~A: ~A"
                          sname e)
      (note t :debug "Fetching image for furre ~A." sname)
      (let* ((stream (drakma:http-request (cl-furcadia:url image)
                                          :want-stream t :force-binary t))
             (png (pngload:load-stream (flex:flexi-stream-stream stream))))
        (note t :debug
              "Successfully fetched image ~D for furre ~A."
              (cl-furcadia:iid image) sname)
        (setf (cl-furcadia:data image) png)
        (push image (gethash 'images output))
        (when-let ((picker (raptor-picker)))
          (signal! picker (image-downloaded string int)
                   sname (cl-furcadia:iid image)))))))
