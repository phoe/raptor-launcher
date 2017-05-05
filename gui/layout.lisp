;;;; layout.lisp

(in-package :furcadia-launcher-gui)
(in-readtable :qtools)

;;;; MAIN LAYOUT

(defmacro set-launcher-layout ()
  `(mapc (curry #'apply #'q+:add-widget layout)
         `(;;; IMAGE
           (,image 0 0 11 1)
           ;; BOXEN
           (,news-box 0 1 11 1)
           (,config-box 0 1 11 1)
           (,editor-box 0 1 11 1)
           (,debug-box 0 1 11 1)
           (,help-box 0 1 11 1)
           (,chars-box 0 1 11 1)
          ;;; BUTTONS
           (,button-news 0 2)
           (,button-config 1 2)
           (,button-chars 2 2)
           (,button-editor 3 2)
           (,button-debug 4 2)
           (,button-help 5 2)
           (,button-play 8 2)
           (,button-sync 9 2)
           (,button-quit 10 2))))

(defmacro disable-buttons ()
  `(setf (q+:enabled button-news) nil))

(define-subwidget (launcher layout) (q+:make-qgridlayout)
  (setf (q+:window-title launcher) (format nil "Raptor Launcher ~A" *version*)
        (q+:fixed-size launcher) (values 600 420)
        (q+:layout central-widget) layout
        (q+:focus) character-list)
  (set-launcher-layout)
  (launcher-hide-all)
  (disable-buttons)
  (q+:show chars-box))

(defun error-text (symbol)
  (format
   nil
   "<b>Woops - I got an error.</b><br />~A"
   (case symbol
     (accounts "Check that you properly input all emails and passwords.")
     (login "Check that your credentials are valid.<br />
\(I got at least one login failure.)")
     (fetch "I have failed to connect to FurEd.<br />
Check that your network connection is fully working.")
     (chars "I have failed to fetch some of the characters.<br />
Check that your network connection is fully working.")
     (t "Something went wrong, and I don't know what exactly.<br />
Check the logs for more information."))))

;;;; INITIALIZE

(define-signal (launcher initialization-required) ())

(define-slot (launcher initialize) ()
  (declare (connected launcher (initialization-required))
           (connected button-sync (clicked)))
  (signal! launcher (hide-chars-witty-text))
  (setf (q+:focus) button-config
        (q+:enabled button-play) nil
        (q+:enabled config-reset) nil
        (q+:enabled config-save) nil)
  (q+:hide character-list)
  (q+:hide description-preview)
  (q+:show hide-chars-box)
  (signal! launcher (begin-initialization)))

(defmacro with-slot-thread (&body body)
  `(bt:make-thread
    (lambda ()
      (handler-case (progn ,@body)
        (error (e) (note :error "Thread finished with an error: ~A" e))))))

;;;; VERIFY-CONFIG-FILE

(define-signal (launcher begin-initialization) ())

(define-slot (launcher verify-config-file) ()
  (declare (connected launcher (begin-initialization)))
  (with-slot-thread
    (if (furcadia-launcher::algorithm-verify-config-file)
        (signal! launcher (verify-config-file-okay))
        (signal! launcher (algorithm-failure string) (error-text 'accounts)))))

;;;; LOGIN-ALL-ACCOUNTS

(define-signal (launcher verify-config-file-okay) ())

(define-slot (launcher login-all-accounts) ()
  (declare (connected launcher (verify-config-file-okay)))
  (signal! launcher (hide-chars-witty-text))
  (with-slot-thread
    (if (furcadia-launcher::algorithm-login-all-accounts)
        (signal! launcher (login-all-accounts-okay))
        (signal! launcher (algorithm-failure string) (error-text 'login)))))

;;;; FETCH-ALL-ACCOUNTS

(define-signal (launcher login-all-accounts-okay) ())

(define-slot (launcher fetch-all-accounts) ()
  (declare (connected launcher (login-all-accounts-okay)))
  (signal! launcher (hide-chars-witty-text))
  (with-slot-thread
    (if (furcadia-launcher::algorithm-fetch-all-accounts)
        (signal! launcher (fetch-all-accounts-okay))
        (signal! launcher (algorithm-failure string) (error-text 'fetch)))))

;;;; FETCH-ALL-CHARACTERS

(define-signal (launcher fetch-all-accounts-okay) ())

(define-slot (launcher fetch-all-characters) ()
  (declare (connected launcher (fetch-all-accounts-okay)))
  (signal! launcher (hide-chars-witty-text))
  (with-slot-thread
    (if (furcadia-launcher::algorithm-fetch-all-characters)
        (signal! launcher (fetch-all-characters-okay))
        (signal! launcher (algorithm-failure string) (error-text 'chars)))))

;;;; FINISH-INITIALIZATION

(define-signal (launcher fetch-all-characters-okay) ())

(define-slot (launcher finish-initialization) ()
  (declare (connected launcher (fetch-all-characters-okay)))
  (q+:show character-list)
  (q+:show description-preview)
  (q+:hide hide-chars-box)
  (setf (q+:text hide-chars-box)
        (hide-chars-box-text "All clear!")
        (q+:enabled button-play) nil
        (q+:enabled config-reset) t
        (q+:enabled config-save) t
        (q+:enabled button-play) t
        (q+:sorting-enabled character-list) nil
        (q+:row-count character-list) 0)
  (mapc (curry #'add-character-to-list character-list)
        (furcadia-launcher::state-last-logins))
  (q+:clear-selection character-list)
  (setf (q+:sorting-enabled character-list) t)
  ;; TODO use trivial-garbage later
  #+sbcl (sb-ext:gc :full t))

;;;; EMERGENCY-UNLOCK

(define-signal (launcher algorithm-failure) (string))

(define-slot (launcher emergency-unlock) ((message string))
  (declare (connected launcher (algorithm-failure string)))
  (setf (q+:text hide-chars-box)
        (hide-chars-box-text message)
        (q+:enabled button-play) nil
        (q+:enabled config-reset) t
        (q+:enabled config-save) t)
  ;; TODO use trivial-garbage later
  #+sbcl (sb-ext:gc :full t))

;;;; MAIN

(defvar *launcher* nil)

;;;; TODO move this to a separate file in GUI, loaded last
(defun main ()
  (unwind-protect
       (with-main-window (launcher 'launcher)
         (note :info "Raptor Launcher GUI started.")
         (setf *launcher* launcher)
         (furcadia-launcher::algorithm-load-config-file)
         (bt:make-thread
          (lambda ()
            (handler-case
                (progn
                  (note :info "Downloading news.")
                  (furcadia-launcher::get-all-newsf)
                  (signal! launcher (enable-news)))
              (error (e)
                (note :error "Error while downloading news: ~A" e))))
          :name "Raptor Launcher news downloader")
         (reset-config launcher)
         (signal! launcher (initialization-required)))
    (setf *launcher* nil)))
