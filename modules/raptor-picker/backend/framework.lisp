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
                     furres-costumes
                     furres-portraits
                     furres-specitags)
    ;; (furres-images -> #'dl-image -> (images *))
    ;; (furres-costumes -> #'dl-costume -> (costumes *))
    ;; (furres-portraits -> #'dl-portrait -> (portraits *))
    ;; (furres-specitags -> #'dl-specitag -> (specitags *)) ;; TODO rework this
    ))

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
    (notbbe t :debug "Attempting to log in as ~A." email)
    (with-log-on-error (e "Failed to fetch account ~A: ~A" email e)
      (let ((cookie-jar (cl-furcadia/ws:login email password)))
        (unless cookie-jar
          (raptor-picker-error "Invalid credentials for email ~A." email))
        (note t :debug "Successfully logged in as ~A." email)
        (let ((data (list email cookie-jar)))
          (push data (gethash 'cookie-jars output)))
        (when-let ((picker (raptor-picker)))
          (signal! picker (login-completed string) email))))))

(defun test-login ()
  (let ((output (make-hash-table)))
    (login (plist-hash-table `(credentials ((,*test-email* ,*test-password*))))
           output)
    (hash-table-plist output)))

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

(defun test-dl-account ()
  (let ((output (make-hash-table)))
    (dl-account (plist-hash-table (test-login)) output)
    (hash-table-plist output)))

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
              (note t :warn "Successfully fetched furre ~A, but unknown keywords
were encountered (possible bug?): ~A" unknowns)
              (note t :debug "Successfully fetched furre ~A." sname))
          (setf (cl-furcadia:last-login furre) last-login
                (cl-furcadia:account furre) account)
          (push furre (gethash 'furres output))
          (push furre (gethash 'furres-images output))
          (push furre (gethash 'furres-costumes output))
          (push furre (gethash 'furres-portraits output))
          (push furre (gethash 'furres-specitags output))
          (when-let ((picker (raptor-picker)))
            (let ((nspecitags (length (cl-furcadia:specitags furre)))
                  (nportraits (length (cl-furcadia:portraits furre)))
                  (ncostumes (length (cl-furcadia:costumes furre))))
              (signal! picker (furre-downloaded string int int int)
                       sname nspecitags nportraits ncostumes))))))))

(defun test-dl-furre ()
  (let* ((input (plist-hash-table (test-dl-account)))
         (accounts (gethash 'accounts-furres input)))
    (loop for account in accounts
          for new-input = (make-hash-table)
          for new-output = (make-hash-table)
          do (push account (gethash 'accounts-furres new-input))
             (dl-furre new-input new-output)
          append (gethash 'furres new-output))))
