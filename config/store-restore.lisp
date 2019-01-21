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

(defgeneric restore-object (type config-path))

(defun restore-all-objects (type config-path)
  (dolist (key (hash-table-keys (apply #'config config-path)))
    (restore-object type (append config-path (list key)))))

(defmacro with-config-accessors ((&rest variables) config-path &body body)
  (let ((bindings (mapcar (lambda (x) `(,x (config ,@config-path
                                                   ,(make-keyword x))))
                          variables)))
    `(symbol-macrolet ,bindings ,@body)))

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

;; TODO remove password from account object
(defmethod restore-object ((type (eql :account)) config-path)
  (let ((email (lastcar config-path)))
    (with-config-accessors (id main gd session) (:config :accounts email)
      (print gd)
      (let* ((account (make-instance 'standard-account
                                     :email email :id id
                                     :gd gd :session session)))
        ;; (setf (cookie-jar-of account)
        ;;       (restore-object :cookie-jar (append config-path (list :cookies))))
        ;; furres
        ;; main
        ;; cookie-jar
        account))))

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
            (costume-config :wings) (wings costume) ;; TODO proper wing type
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

;;; STANDARD-SPECITAG

(defmethod store-object ((specitag standard-specitag) &key email sname)
  (let ((sid (sid specitag)))
    (flet (((setf specitag-config) (new-value &rest path)
             (apply #'(setf config) new-value :config :accounts
                    email :furres sname :specitags sid path)))
      (setf (specitag-config :remappedp) (remappedp specitag)))))

;;; STANDARD-PORTRAIT

(defmethod store-object ((portrait standard-portrait) &key email sname)
  (let ((pid (pid portrait)))
    (flet (((setf portrait-config) (new-value &rest path)
             (apply #'(setf config) new-value :config :accounts
                    email :furres sname :portraits pid path)))
      (setf (portrait-config :portrait-type) (portrait-type portrait)
            (portrait-config :remappedp) (remappedp portrait)))))

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
