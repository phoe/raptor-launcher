;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; store-restore.lisp

(in-package :raptor-launcher/config)

(defgeneric store-object (object &key))

(defvar *in-transaction* nil)

(defmethod store-object :around (object &key)
  (if *in-transaction*
      (call-next-method)
      (let ((*in-transaction* t))
        (with-config-transaction () (call-next-method)))))

(defgeneric restore-object (type config-path &key)) ;; TODO &key

(defmethod restore-object :around (type config-path &key)
  (and (apply #'econfig config-path) (call-next-method)))

(defun restore-all-objects (type config-path &rest keyword-args)
  (when-let ((hash-table (apply #'config config-path)))
    (uiop:while-collecting (collect)
      (dolist (key (hash-table-keys hash-table))
        (collect (apply #'restore-object type (append config-path (list key))
                        keyword-args))))))

(trivial-indent:define-indentation with-config-accessors
    ((&whole 4 &rest 1) 4 &rest 2))

(defmacro with-config-accessors ((&rest variables) config-path &body body)
  (flet ((generate (x) `(,x (apply #'config (append ,config-path
                                                    (list ,(make-keyword x)))))))
    (let ((bindings (mapcar #'generate variables)))
      `(symbol-macrolet ,bindings ,@body))))

(defgeneric postprocess (object)
  (:method :around (object) (call-next-method) object)
  (:method ((furre furre))
    (push furre (furres (account furre))))
  (:method ((portrait portrait))
    (push portrait (portraits (furre portrait))))
  (:method ((specitag specitag))
    (push specitag (specitags (furre specitag))))
  (:method ((image image))
    (push image (images (furre image))))
  (:method ((costume costume))
    (when (specitag costume)
      (setf (specitag costume)
            (find (specitag costume)
                  (specitags (furre costume))
                  :key #'sid)))
    (when (afk-portrait costume)
      (setf (afk-portrait costume)
            (find (afk-portrait costume)
                  (portraits (furre costume))
                  :key #'pid)))
    (when (portrait costume)
      (setf (portrait costume)
            (find (portrait costume)
                  (portraits (furre costume))
                  :key #'pid)))
    (push costume (costumes (furre costume))))
  (:method ((account account))
    (setf (main account)
          (find (main account) (furres account)
                :key #'shortname :test #'string=))))

;;; STANDARD-ACCOUNT

(defmethod store-object ((account standard-account) &key)
  (let ((email (email account)))
    (flet (((setf account-config) (new-value &rest path)
             (apply #'(setf config) new-value :config :accounts email path)))
      (setf (account-config :id) (id account)
            (account-config :main) (shortname (main account))
            (account-config :gd) (gd account)
            (account-config :session) (session account)))
    (mapc (rcurry #'store-object :email email) (furres account))
    (store-object (cookie-jar-of account) :email email)))

;; TODO remove password slot from account object
(defmethod restore-object ((type (eql :account)) config-path &key)
  (let ((email (lastcar config-path)))
    (with-config-accessors (id main gd session) config-path
      (let* ((account (make-instance 'standard-account
                                     :email email :id id :main main
                                     :gd gd :session session)))
        (setf (cookie-jar-of account)
              (restore-object :cookie-jar (append config-path (list :cookies))
                              :account account)
              (furres account)
              (restore-all-objects :furre (append config-path (list :furres))
                                   :account account))
        (postprocess account)))))

;;; COOKIE-JAR

(defmethod store-object ((cookie-jar drakma:cookie-jar) &key email)
  (dolist (cookie (drakma:cookie-jar-cookies cookie-jar))
    (flet (((setf cookie-config) (new-value &rest path)
             (apply #'(setf config) new-value :config :accounts
                    email :cookies
                    (drakma:cookie-domain cookie)
                    (drakma:cookie-name cookie) path)))
      (setf (cookie-config :password) (drakma:cookie-value cookie)
            (cookie-config :path) (drakma:cookie-path cookie)
            (cookie-config :expires) (drakma:cookie-expires cookie)
            (cookie-config :securep) (drakma:cookie-securep cookie)
            (cookie-config :http-only-p) (drakma:cookie-http-only-p cookie)))))

(defmethod restore-object ((type (eql :cookie-jar)) config-path &key)
  (let ((cookies '()))
    (dolist (domain (hash-table-keys (apply #'config config-path)))
      (let ((config-path (append config-path (list domain))))
        (dolist (name (hash-table-keys (apply #'config config-path)))
          (with-config-accessors (password path expires securep http-only-p)
              (append config-path (list name))
            (push (make-instance 'drakma:cookie
                                 :domain domain :name name :value password
                                 :path path :expires expires :securep securep
                                 :http-only-p http-only-p)
                  cookies)))))
    (make-instance 'drakma:cookie-jar :cookies cookies)))

;;; STANDARD-FURRE

(defmethod store-object ((furre standard-furre) &key email)
  (let ((sname (shortname furre)))
    (flet (((setf furre-config) (new-value &rest path)
             (apply #'(setf config) new-value :config :accounts
                    email :furres sname path)))
      (setf (furre-config :uid) (uid furre)
            (furre-config :name) (name furre)
            (furre-config :last-login) (last-login furre)
            (furre-config :digos) (mapcar #'index (digos furre))
            (furre-config :lifers) (mapcar #'index (lifers furre))
            (furre-config :active-costume) (cid (active-costume furre))))
    (mapc (rcurry #'store-object :email email :sname sname) (costumes furre))
    (mapc (rcurry #'store-object :email email :sname sname) (specitags furre))
    (mapc (rcurry #'store-object :email email :sname sname) (portraits furre))
    (mapc (rcurry #'store-object :email email :sname sname) (images furre))))

(defmethod restore-object ((type (eql :furre)) config-path &key account)
  (with-config-accessors (uid name last-login digos lifers active-costume)
      config-path
    (flet ((find-digo (x) (gethash x cl-furcadia:*digos*))
           (restore-type (type config-tail furre)
             (restore-all-objects type
                                  (append config-path (list config-tail))
                                  :furre furre)))
      (let* ((furre (make-instance 'standard-furre
                                   :account account :uid uid :name name
                                   :last-login last-login
                                   :digos (mapcar #'find-digo digos)
                                   :lifers (mapcar #'find-digo lifers))))
        (setf (specitags furre) (restore-type :specitag :specitags furre)
              (images furre) (restore-type :image :images furre)
              (portraits furre) (restore-type :portrait :portraits furre)
              (costumes furre) (restore-type :costume :costumes furre)
              (active-costume furre)
              (find active-costume (costumes furre) :key #'cid))
        (postprocess furre)))))

;;; STANDARD-SPECITAG

(defmethod store-object ((specitag standard-specitag) &key email sname)
  (let ((sid (sid specitag)))
    (flet (((setf specitag-config) (new-value &rest path)
             (apply #'(setf config) new-value :config :accounts
                    email :furres sname :specitags sid path)))
      (setf (specitag-config :remappedp) (remappedp specitag)))))

(defmethod restore-object ((type (eql :specitag)) config-path &key furre)
  (with-config-accessors (remappedp) config-path
    (let* ((sid (lastcar config-path))
           (specitag (make-instance 'standard-specitag :sid sid :furre furre
                                                       :remappedp remappedp)))
      (postprocess specitag))))

;;; STANDARD-PORTRAIT

(defmethod store-object ((portrait standard-portrait) &key email sname)
  (let ((pid (pid portrait)))
    (flet (((setf portrait-config) (new-value &rest path)
             (apply #'(setf config) new-value :config :accounts
                    email :furres sname :portraits pid path)))
      (setf (portrait-config :portrait-type) (portrait-type portrait)
            (portrait-config :remappedp) (remappedp portrait)))))

(defmethod restore-object ((type (eql :portrait)) config-path &key furre)
  (with-config-accessors (portrait-type remappedp) config-path
    (let* ((pid (lastcar config-path))
           (portrait (make-instance 'standard-portrait
                                    :pid pid :portrait-type portrait-type
                                    :furre furre :remappedp remappedp)))
      (postprocess portrait))))

;;; STANDARD-IMAGE

(defmethod store-object ((image standard-image) &key email sname)
  (let ((iid (iid image)))
    (flet (((setf image-config) (new-value &rest path)
             (apply #'(setf config) new-value :config :accounts
                    email :furres sname :images iid path)))
      (setf (image-config :timestamp) (timestamp image)
            (image-config :url) (url image)
            (image-config :eye-level) (eye-level image)
            (image-config :sfwp) (sfwp image)))))

(defmethod restore-object ((type (eql :image)) config-path &key furre)
  (with-config-accessors (timestamp url eye-level sfwp) config-path
    (let* ((iid (lastcar config-path))
           (image (make-instance 'standard-image
                                 :iid iid :timestamp timestamp :url url
                                 :eye-level eye-level :furre furre :sfwp sfwp)))
      (postprocess image))))

;;; STANDARD-COSTUME

(defmethod store-object ((costume standard-costume) &key email sname)
  (let ((cid (cid costume)))
    (flet (((setf costume-config) (new-value &rest path)
             (apply #'(setf config) new-value :config :accounts
                    email :furres sname :costumes cid path)))
      (setf (costume-config :name) (name costume)
            (costume-config :rating) (rating costume)
            (costume-config :scale) (scale costume)
            (costume-config :ordinal) (ordinal costume)
            (costume-config :description) (description costume)
            (costume-config :color-code) (color-code costume)
            (costume-config :digo) (when (digo costume)
                                     (index (digo costume)))
            (costume-config :specitag) (when (specitag costume)
                                         (sid (specitag costume)))
            (costume-config :wings) (wings costume)
            (costume-config :portrait) (when (portrait costume)
                                         (pid (portrait costume)))
            (costume-config :auto-response) (auto-response costume)
            (costume-config :auto-response-p) (auto-response-p costume)
            (costume-config :afk-description) (afk-description costume)
            (costume-config :afk-whisper) (afk-whisper costume)
            (costume-config :afk-color-code) (afk-color-code costume)
            (costume-config :afk-digo) (when (afk-digo costume)
                                         (index (afk-digo costume)))
            (costume-config :afk-wings) (afk-wings costume)
            (costume-config :afk-portrait) (when (afk-portrait costume)
                                             (pid (afk-portrait costume)))
            (costume-config :afk-time) (afk-time costume)
            (costume-config :afk-max-time) (afk-max-time costume)))))

(defmethod restore-object ((type (eql :costume)) config-path &key furre)
  (with-config-accessors
      (name rating scale ordinal description color-code digo specitag wings
       portrait auto-response auto-response-p afk-description afk-whisper
       afk-color-code afk-digo afk-wings afk-portrait afk-time afk-max-time)
      config-path
    (flet ((maybe-find-digo (x)
             (when x (gethash-or-die x cl-furcadia:*digos*))))
      (let* ((cid (lastcar config-path))
             (costume (make-instance
                       'standard-costume
                       :cid cid :furre furre :name name :rating rating
                       :scale scale :ordinal ordinal :description description
                       :color-code color-code :specitag specitag :wings wings
                       :portrait portrait :auto-response auto-response
                       :auto-response-p auto-response-p
                       :afk-description afk-description :afk-whisper afk-whisper
                       :afk-color-code afk-color-code :afk-wings afk-wings
                       :afk-time afk-time :afk-max-time afk-max-time
                       :afk-portrait afk-portrait
                       :digo (maybe-find-digo digo)
                       :afk-digo (maybe-find-digo afk-digo))))
        (postprocess costume)))))
