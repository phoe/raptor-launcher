;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; config-widget.lisp

(in-package :raptor-launcher/raptor-picker)
(in-readtable :qtools)

;;; Configuration widget

;;; TODO add traces everywhere, EVERYWHERE

(define-widget config-widget (qwidget)
  ((module :accessor module
           :initarg :module)))

(define-qt-constructor (config-widget)
  (loop with accounts-data = (config :config :accounts)
        for index being the hash-keys of accounts-data
        for email = (config :config :accounts index :email)
        for password = (config :config :accounts index :password)
        do (add-account accounts config-widget email password)))

(define-subwidget (config-widget layout) (q+:make-qgridlayout)
  (setf (q+:layout config-widget) layout))

(define-subwidget (config-widget title) (q+:make-qlabel "Accounts")
  (q+:add-widget layout title 0 0 1 2)
  (setf (q+:style-sheet title) "font-weight: bold;"))

(define-subwidget (config-widget separator) (q+:make-qframe)
  (q+:add-widget layout separator 1 0 1 2)
  (setf (q+:frame-shape separator) (q+:qframe.hline)
        (q+:frame-shadow separator) (q+:qframe.sunken)))

(define-subwidget (config-widget add-button) (q+:make-qpushbutton "Add")
  (q+:add-widget layout add-button 2 0 1 1))

(define-subwidget (config-widget delete-button) (q+:make-qpushbutton "Delete")
  (q+:add-widget layout delete-button 2 1 1 1))

(define-subwidget (config-widget accounts) (q+:make-qwidget)
  (q+:add-widget layout accounts 3 0 1 2))

(define-subwidget (config-widget accounts-layout) (q+:make-qvboxlayout)
  (setf (q+:layout accounts) accounts-layout))

(define-slot (config-widget add-account) ()
  (declare (connected add-button (clicked)))
  (add-account accounts config-widget))

(define-slot (config-widget delete-acccount) ()
  (declare (connected delete-button (clicked)))
  (remove-account accounts))

(define-slot (config-widget update-accounts) ()
  (remconfig :config :accounts)
  (let ((accounts (get-all-accounts accounts)))
    (loop for n from 1
          for (email password) in accounts
          do (setf (config :config :accounts n :email) email)
             (setf (config :config :accounts n :password) password))))

(defun new-account-number (widget)
  (let ((accounts (find-children widget "QLineEdit")))
    (1+ (/ (length accounts) 2))))

(defun add-account (widget config-widget &optional email-text password-text)
  (let* ((layout (q+:layout widget))
         (n (new-account-number widget))
         (email (q+:make-qlineedit))
         (password (q+:make-qlineedit)))
    (setf (q+:echo-mode password) (q+:qlineedit.password)
          (q+:placeholder-text email) (format nil "Account ~D: email" n)
          (q+:placeholder-text password) (format nil "Account ~D: password" n))
    (connect! email (editing-finished) config-widget (update-accounts))
    (connect! password (editing-finished) config-widget (update-accounts))
    (when email-text (setf (q+:text email) email-text))
    (when password-text (setf (q+:text password) password-text))
    (q+:add-widget layout email)
    (q+:add-widget layout password)))

(defun get-all-accounts (widget)
  (let* ((children (find-children widget "QLineEdit"))
         (credentials (mapcar #'q+:text children)))
    (loop for (email password) on credentials by #'cddr
          collect (list (string-downcase email) password))))

(defun remove-account (widget)
  (when (< 1 (new-account-number widget))
    (let ((accounts (find-children widget "QLineEdit")))
      (finalize (last accounts 2))
      (finalize (last accounts 1)))))
