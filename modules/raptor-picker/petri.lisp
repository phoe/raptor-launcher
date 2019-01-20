;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; framework.lisp

(in-package :raptor-launcher/raptor-picker)
(in-readtable :qtools)

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

;;; Petri net definition

(defun make-picker-petri-net ()
  (let ((petri-net
            (threaded-petri-net ()
              (update-digo-data-p -> #'dl-digo-data -> new-digo-data-p)
              (new-digo-data-p old-digo-data-p -> #'save-digo-data)
              (credentials -> #'login -> cookie-jars
                           -> #'dl-account -> accounts (accounts-furres *))
              (accounts-furres (old-digo-data-p !)
                               -> #'dl-furre
                               -> furres
                               (furres-costumes *)
                               (furres-portraits *)
                               (furres-specitags *)
                               furres-images)
              (furres-costumes -> #'dl-costume -> costumes)
              (furres-portraits -> #'dl-portrait -> portraits)
              (furres-specitags -> #'dl-specitag -> specitags)
              (furres-images -> #'dl-image-list -> (image-metadata *))
              (image-metadata -> #'dl-image -> images))))
    (bag-insert (bag-of petri-net 'update-digo-data-p) t)
    (bag-insert (bag-of petri-net 'old-digo-data-p) t)
    petri-net))

(defvar *test-email*)
(defvar *test-password*)

(defun test-petri ()
  (let ((petri (make-picker-petri-net)))
    (bag-insert (bag-of petri 'credentials)
                `(,*test-email* ,*test-password*))
    petri))

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
                (cl-furcadia:account furre) account
                (cl-furcadia:digos furre)
                (mapcar (rcurry #'gethash-or-die cl-furcadia:*digos*)
                        (cl-furcadia:digos furre))
                (cl-furcadia:lifers furre)
                (mapcar (rcurry #'gethash-or-die cl-furcadia:*digos*)
                        (cl-furcadia:lifers furre)))
          (push furre (gethash 'furres output))
          (push furre (gethash 'furres-images output))
          (dotimes (i (length (cl-furcadia:costumes furre)))
            (push (list furre (pop (cl-furcadia:costumes furre)))
                  (gethash 'furres-costumes output)))
          (dotimes (i (length (cl-furcadia:portraits furre)))
            (push (list furre (pop (cl-furcadia:portraits furre)))
                  (gethash 'furres-portraits output)))
          (dotimes (i (length (cl-furcadia:specitags furre)))
            (push (list furre (pop (cl-furcadia:specitags furre)))
                  (gethash 'furres-specitags output)))
          (when-let ((picker (raptor-picker)))
            (let ((nspecitags (length (cl-furcadia:specitags furre)))
                  (nportraits (length (cl-furcadia:portraits furre)))
                  (ncostumes (length (cl-furcadia:costumes furre))))
              (signal! picker (furre-downloaded string int int int)
                       sname nspecitags nportraits ncostumes)
              (signal! picker (costume-downloaded string int)
                       sname 0))))))))

;;; DL-COSTUME

(defun dl-costume (input output)
  (destructuring-bind (furre costume) (pop (gethash 'furres-costumes input))
    (let ((sname (cl-furcadia:shortname furre))
          (cookie-jar (cl-furcadia:cookie-jar-of (cl-furcadia:account furre))))
      (with-log-on-error
          (e "Failed to fetch costume ~A for furre ~A: ~A" costume sname e)
        (etypecase costume
          (cl-furcadia:costume
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
        (multiple-value-bind (portrait data)
            (cl-furcadia/ws:fetch-portrait pid cookie-jar)
          (note t :debug "Successfully fetched portrait ~A for furre ~A."
                pid sname)
          (setf (cl-furcadia:furre portrait) furre
                (cl-furcadia:image-data portrait *dl-path*) data)
          (push portrait (gethash 'portraits output))
          (when-let ((picker (raptor-picker)))
            (signal! picker (portrait-downloaded string int)
                     sname pid)))))))

;;; DL-SPECITAG

(defun dl-specitag (input output)
  (destructuring-bind (furre (sid remappedp))
      (pop (gethash 'furres-specitags input))
    (let ((sname (cl-furcadia:shortname furre)))
      (with-log-on-error (e "Failed to fetch specitag ~A for furre ~A: ~A"
                            sid sname e)
        (note t :debug "Fetching specitag ~A for furre ~A." sid sname)
        (multiple-value-bind (specitag data) (cl-furcadia/ws:fetch-specitag sid)
          (note t :debug "Successfully fetched specitag ~A for furre ~A."
                sid sname)
          (setf (cl-furcadia:furre specitag) furre
                (cl-furcadia:remappedp specitag) remappedp
                (cl-furcadia:image-data specitag *dl-path*) data)
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

;; TODO implement image caching somewhere based on sname, iid, and timestamp
(defun dl-image (input output)
  (let* ((image (pop (gethash 'image-metadata input)))
         (iid (cl-furcadia:iid image))
         (timestamp-string (princ-to-string (cl-furcadia:timestamp image)))
         (sname (cl-furcadia:shortname (cl-furcadia:furre image))))
    ;; TODO move images-dir to config.lisp
    (let* ((images-dir (merge-pathnames "images/" *dl-path*))
           (image-dir (merge-pathnames (uiop:strcat sname "/") images-dir))
           (data-path (merge-pathnames (uiop:strcat timestamp-string ".png")
                                       image-dir)))
      (ensure-directories-exist image-dir)
      (if (probe-file data-path)
          (note t :debug "Image ~D for furre ~A found in cache." iid sname)
          (with-log-on-error (e "Failed to fetch image ~D for furre ~A: ~A"
                                iid sname e)
            (note t :debug "Fetching image ~D for furre ~A." iid sname)
            (let* ((stream (drakma:http-request (cl-furcadia:url image)
                                                :want-stream t :force-binary t))
                   (flexi-stream (flex:flexi-stream-stream stream))
                   (png (pngload:load-stream flexi-stream :flatten t)))
              (note t :debug
                    "Successfully fetched image ~D for furre ~A."
                    iid sname)
              (setf (cl-furcadia:image-data image *dl-path*) png)))))
    (push image (gethash 'images output))
    (when-let ((picker (raptor-picker)))
      (signal! picker (image-downloaded string int)
               sname (cl-furcadia:iid image)))))

;;; Load/save digo data

(defun dl-digo-data (input output)
  (declare (ignore input))
  (with-log-on-error (e "Failed to update digo data: ~A" e)
    (note t :debug "Updating digo data.")
    (cl-furcadia:update-digo-data)
    (note t :debug "Successfully updated digo data: ~D digos available."
          (hash-table-count cl-furcadia:*digos*))
    (push t (gethash 'new-digo-data-p output))))

(defun save-digo-data (&optional input output)
  (declare (ignore input output))
  (with-log-on-error (e "Failed to save digo data to disk: ~A" e)
    (note t :debug "Saving digo data to disk.")
    (with-output-to-file (stream *digo-path* :if-exists :supersede)
      (print-hash-table-readably cl-furcadia:*digos* stream))
    (note t :debug "Successfully saved digo data to disk.")))

(defun load-digo-data (&optional input output)
  (declare (ignore input output))
  (with-log-on-error (e "Failed to load digo data from disk: ~A" e)
    (note t :debug "Loading digo data from disk.")
    (with-input-from-file (stream *digo-path*)
      (setf cl-furcadia:*digos* (read stream)))
    (note t :debug "Successfully loaded digo data from disk.")))

;;; Postprocessing

(defun postprocess-picker-petri-net (petri-net)
  (map nil
       (lambda (furre)
         (push furre (cl-furcadia:furres (cl-furcadia:account furre))))
       (bag-contents (bag-of petri-net 'furres)))
  (map nil
       (lambda (portrait)
         (push portrait (cl-furcadia:portraits (cl-furcadia:furre portrait))))
       (bag-contents (bag-of petri-net 'portraits)))
  (map nil
       (lambda (specitag)
         (push specitag (cl-furcadia:specitags (cl-furcadia:furre specitag))))
       (bag-contents (bag-of petri-net 'specitags)))
  (map nil
       (lambda (image)
         (push image (cl-furcadia:images (cl-furcadia:furre image))))
       (bag-contents (bag-of petri-net 'images)))
  (map nil
       (lambda (costume)
         (when (cl-furcadia:specitag costume)
           (setf (cl-furcadia:specitag costume)
                 (find (cl-furcadia:specitag costume)
                       (cl-furcadia:specitags (cl-furcadia:furre costume))
                       :key #'cl-furcadia:sid))
           (setf (cl-furcadia:afk-portrait costume)
                 (find (cl-furcadia:afk-portrait costume)
                       (cl-furcadia:portraits (cl-furcadia:furre costume))
                       :key #'cl-furcadia:pid))
           (setf (cl-furcadia:portrait costume)
                 (find (cl-furcadia:portrait costume)
                       (cl-furcadia:portraits (cl-furcadia:furre costume))
                       :key #'cl-furcadia:pid)))
         (push costume (cl-furcadia:costumes (cl-furcadia:furre costume))))
       (bag-contents (bag-of petri-net 'costumes)))
  (note t :trace "Cleaning stored configuration for all accounts.")
  (remconfig :config :accounts)
  (map nil
       (lambda (account)
         (setf (cl-furcadia:main account)
               (find (cl-furcadia:main account) (furres account)
                     :key #'cl-furcadia:shortname :test #'string=))
         (with-log-on-error (e "Error while storing configuration for account
~A: ~A" (cl-furcadia:email account) e)
           (note t :trace "Storing configuration for account ~A."
                 (cl-furcadia:email account))
           (store-object account) ;; TODO uncomment
           ))
       (bag-contents (bag-of petri-net 'accounts)))
  (note t :trace "Successfully stored configuration for all accounts.")
  petri-net)
