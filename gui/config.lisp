;;;; config.lisp

(in-package :furcadia-launcher-gui)
(in-readtable :qtools)

;;;; TODO clarify this - change to Path to main Furcadia executable:
(define-subwidget (launcher config-furcadia-path) (q+:make-qlineedit)
  (setf (q+:placeholder-text config-furcadia-path)
        "e.g. C:\\Program Files (x86)\\Furcadia\\"))

(define-subwidget (launcher config-add-account)
    (q+:make-qpushbutton "Add"))

(define-subwidget (launcher config-delete-account)
    (q+:make-qpushbutton "Delete"))

(define-subwidget (launcher config-accounts-layout) (q+:make-qvboxlayout))

(define-subwidget (launcher config-accounts-widget) (q+:make-qwidget)
  (setf (q+:layout config-accounts-widget) config-accounts-layout))

(define-slot (launcher add-account) ()
  (declare (connected config-add-account (clicked)))
  (add-credential-input config-accounts-widget))

(define-slot (launcher delete-acccount) ()
  (declare (connected config-delete-account (clicked)))
  (remove-credential-input config-accounts-widget))

(defparameter *launcher-keep-text*
  "Keep Launcher running after launching Furcadia?")

(define-subwidget (launcher config-keep-checkbox) (q+:make-qcheckbox))

(defparameter *launcher-sync-text*
  "Skip synchronizing with the server on startup?")

(define-subwidget (launcher config-sync-checkbox) (q+:make-qcheckbox))

(define-subwidget (launcher config-contents) (q+:make-qgridlayout)
  (setf (q+:contents-margins config-contents) (values 0 0 0 0)
        (q+:alignment config-contents) (q+:qt.align-top))
  (mapc (curry #'apply #'q+:add-widget config-contents)
        `((,(q+:make-qlabel *launcher-sync-text*) 0 0 1 4)
          (,config-sync-checkbox 1 0 1 4)
          (,(q+:make-qlabel *launcher-keep-text*) 2 0 1 4)
          (,config-keep-checkbox 3 0 1 4)
          (,(q+:make-qlabel "Path to Furcadia installation folder") 4 0 1 4)
          (,config-furcadia-path 5 0 1 4)
          (,(q+:make-qlabel "Accounts") 6 0 1 4)
          (,config-add-account 7 0 1 2)
          (,config-delete-account 7 2 1 2)
          (,config-accounts-widget 8 0 1 4))))

(define-subwidget (launcher config-contents-widget) (q+:make-qwidget)
  (setf (q+:layout config-contents-widget) config-contents))

(define-subwidget (launcher config-scroll) (q+:make-qscrollarea)
  (setf (q+:widget config-scroll) config-contents-widget
        (q+:widget-resizable config-scroll) t
        (q+:frame-shape config-scroll) (q+:qframe.no-frame)))

(define-subwidget (launcher config-save) (q+:make-qpushbutton "Save")
  (setf (q+:maximum-width config-save) 90
        (q+:minimum-width config-save) 90))

(define-subwidget (launcher config-reset) (q+:make-qpushbutton "Reset")
  (setf (q+:maximum-width config-reset) 90
        (q+:minimum-width config-reset) 90))

(define-slot (launcher save-config) ()
  (declare (connected config-save (clicked)))
  (symbol-macrolet ((config furcadia-launcher::*config*))
    ;; Sync on startup?
    (setf (getf config :skip-sync-on-startup)
          (q+:is-checked config-sync-checkbox))
    ;; Keep running?
    (setf (getf config :keep-running) (q+:is-checked config-keep-checkbox))
    ;; Furcadia path
    (setf (getf config :furcadia-path) (q+:text config-furcadia-path))
    ;; Accounts
    (setf (getf config :accounts)
          (get-all-credentials config-accounts-widget))
    (furcadia-launcher::algorithm-save-config-file)
    ;; TODO fix it - full reinitialization might not be required
    ;; based on the accounts that have changed
    (signal! launcher (synchronize))))

(define-slot (launcher reset-config reset-config) ()
  (declare (connected config-reset (clicked)))
  (symbol-macrolet ((config furcadia-launcher::*config*))
    ;; Sync on startup?
    (setf (q+:checked config-sync-checkbox) (getf config :skip-sync-on-startup))
    ;; Keep running?
    (setf (q+:checked config-keep-checkbox) (getf config :keep-running))
    ;; Furcadia path
    (setf (q+:text config-furcadia-path) (or (getf config :furcadia-path) ""))
    ;; Accounts
    (remove-all-credential-inputs config-accounts-widget)
    (loop for (email password) in (getf config :accounts)
          do (add-credential-input config-accounts-widget email password))
    (note :info "Configuration reset.")))

(define-subwidget (launcher config-separator) (q+:make-qframe)
  (setf (q+:frame-shape config-separator) (q+:qframe.hline)
        (q+:frame-shadow config-separator) (q+:qframe.sunken)))

(define-subwidget (launcher config-layout) (q+:make-qgridlayout)
  (setf (q+:contents-margins config-layout) (values 0 0 0 0))
  (q+:add-widget config-layout config-save 0 0)
  (q+:add-widget config-layout config-reset 0 1)
  (q+:add-widget config-layout config-separator 1 0 1 3)
  (q+:add-widget config-layout config-scroll 2 0 1 3))

(define-subwidget (launcher config-box) (q+:make-qwidget)
  (setf (q+:layout config-box) config-layout))

(defun new-account-number (widget)
  (let ((accounts (find-children widget "QLineEdit")))
    (1+ (/ (length accounts) 2))))

(defun add-credential-input (widget &optional email-text password-text)
  (let* ((layout (q+:layout widget))
         (n (new-account-number widget))
         (email (q+:make-qlineedit))
         (password (q+:make-qlineedit)))
    (setf (q+:echo-mode password) (q+:qlineedit.password)
          (q+:placeholder-text email) (format nil "Account ~D: email" n)
          (q+:placeholder-text password) (format nil "Account ~D: password" n))
    (when email-text (setf (q+:text email) email-text))
    (when password-text (setf (q+:text password) password-text))
    (q+:add-widget layout email)
    (q+:add-widget layout password)))

(defun remove-credential-input (widget)
  (when (< 1 (new-account-number widget))
    (let ((accounts (find-children widget "QLineEdit")))
      (finalize (last accounts 2))
      (finalize (last accounts 1)))))

(defun remove-all-credential-inputs (widget)
  (mapc #'finalize (find-children widget "QLineEdit")))

(defun get-all-credentials (widget)
  (let* ((children (find-children widget "QLineEdit"))
         (credentials (mapcar #'q+:text children)))
    (loop for (email password) on credentials by #'cddr
          collect (list (string-downcase email) password))))

;;;; TODO add an option to nuke configuration in case shit's on fire
