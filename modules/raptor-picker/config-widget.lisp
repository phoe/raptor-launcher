;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; config-widget.lisp

(in-package :raptor-launcher/raptor-picker)
(in-readtable :qtools)

;;; TODO add traces everywhere, EVERYWHERE

(default-config (make-hash-table) :config :credentials)

(define-widget config-widget (qwidget)
  ((module :accessor module
           :initarg :module)))

(define-subwidget (config-widget layout) (q+:make-qgridlayout)
  (setf (q+:layout config-widget) layout))

;;; Header

(define-subwidget (config-widget title) (q+:make-qlabel "Accounts")
  (q+:add-widget layout title 0 0 1 2)
  (setf (q+:style-sheet title) "font-weight: bold;"))

(define-subwidget (config-widget separator) (q+:make-qframe)
  (q+:add-widget layout separator 1 0 1 2)
  (setf (q+:frame-shape separator) (q+:qframe.hline)
        (q+:frame-shadow separator) (q+:qframe.sunken)))

;;; Show account number in character list?

;;; TODO define-config-checkbox for checkboxen
(define-subwidget (config-widget account-number-checkbox)
    (q+:make-qcheckbox "Show account number in character list?")
  (q+:add-widget layout account-number-checkbox 2 0 1 2)
  (let* ((checkedp (config :picker :show-account-number))
         (state (if checkedp (q+:qt.checked) (q+:qt.unchecked))))
    (setf (q+:check-state account-number-checkbox) state)))

(define-slot (config-widget show-account-number) ()
  (declare (connected account-number-checkbox (clicked)))
  (let ((checkedp (q+:is-checked account-number-checkbox)))
    (setf (config :picker :show-account-number) checkedp)
    (signal! (module config-widget)
             (show-account-checkbox-clicked bool) checkedp)))

;;; Show last login in character list?

(define-subwidget (config-widget last-login-checkbox)
    (q+:make-qcheckbox "Show last login in character list?")
  (q+:add-widget layout last-login-checkbox 3 0 1 2)
  (let* ((checkedp (config :picker :show-last-login))
         (state (if checkedp (q+:qt.checked) (q+:qt.unchecked))))
    (setf (q+:check-state last-login-checkbox) state)))

(define-slot (config-widget show-last-login) ()
  (declare (connected last-login-checkbox (clicked)))
  (let ((checkedp (q+:is-checked last-login-checkbox)))
    (setf (config :picker :show-last-login) checkedp)
    (signal! (module config-widget)
             (last-login-checkbox-clicked bool) checkedp)))

;;; Account adding and deleting

(define-subwidget (config-widget add-button) (q+:make-qpushbutton "Add")
  (q+:add-widget layout add-button 4 0 1 1))

(define-subwidget (config-widget delete-button) (q+:make-qpushbutton "Delete")
  (q+:add-widget layout delete-button 4 1 1 1))

(define-subwidget (config-widget accounts) (q+:make-qwidget)
  (q+:add-widget layout accounts 5 0 1 2))

(define-subwidget (config-widget accounts-layout) (q+:make-qvboxlayout)
  (setf (q+:layout accounts) accounts-layout))

(define-slot (config-widget add-account) ()
  (declare (connected add-button (clicked)))
  (add-account accounts config-widget)
  (signal! config-widget (account-update-needed)))

(define-slot (config-widget delete-acccount) ()
  (declare (connected delete-button (clicked)))
  (remove-account accounts)
  (signal! config-widget (account-update-needed)))

(define-signal (config-widget account-update-needed) ())

(define-slot (config-widget update-accounts) ()
  (declare (connected config-widget (account-update-needed)))
  (loop with hash-table = (config :config :credentials)
        for n being the hash-key of hash-table
        do (remconfig :config :credentials n))
  (let ((accounts (get-all-accounts accounts)))
    (loop for n from 1
          for (email password) in accounts
          do (setf (config :config :credentials n :email) email)
             (setf (config :config :credentials n :password) password))))

(define-qt-constructor (config-widget)
  (let ((checkedp (q+:is-checked account-number-checkbox)))
    (signal! (module config-widget)
             (show-account-checkbox-clicked bool) checkedp))
  (let ((checkedp (q+:is-checked last-login-checkbox)))
    (signal! (module config-widget)
             (last-login-checkbox-clicked bool) checkedp))
  (let ((list (loop with accounts-data = (config :config :credentials)
                    for index being the hash-keys of accounts-data
                    for email = (config :config :credentials index :email)
                    for password = (config :config :credentials index :password)
                    collect (list index email password))))
    (loop for (index email password) in (sort list #'< :key #'car)
          do (add-account accounts config-widget email password))))

(defun new-account-number (widget)
  (let ((accounts (find-children widget "QLineEdit")))
    (1+ (/ (length accounts) 2))))

(defun get-all-accounts (widget)
  (let* ((children (find-children widget "QLineEdit"))
         (credentials (mapcar #'q+:text children)))
    (loop for (email password) on credentials by #'cddr
          collect (list (string-downcase email) password))))

(defun add-account (widget config-widget &optional email-text password-text)
  (if email-text
      (note t :trace "Displaying account ~A." email-text)
      (note t :trace "Adding an account to the configuration."))
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

(defun remove-account (widget)
  (note t :trace "Removing an account from the configuration.")
  (when (< 1 (new-account-number widget))
    (let ((accounts (find-children widget "QLineEdit")))
      (finalize (last accounts 2))
      (finalize (last accounts 1)))))
