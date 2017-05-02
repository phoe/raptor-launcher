;;;; config.lisp

(in-package :furcadia-launcher-gui)
(in-readtable :qtools)

(define-subwidget (launcher config-furcadia-path) (q+:make-qlineedit))

(define-subwidget (launcher config-add-account)
    (q+:make-qpushbutton "Add"))

(define-subwidget (launcher config-delete-account)
    (q+:make-qpushbutton "Delete"))

(define-subwidget (launcher config-accounts-layout) (q+:make-qvboxlayout))

(define-subwidget (launcher config-accounts-widget) (q+:make-qwidget)
  (setf (q+:layout config-accounts-widget) config-accounts-layout))

(define-subwidget (launcher config-save) (q+:make-qpushbutton "Save")
  (setf (q+:maximum-width config-save) 90
        (q+:minimum-width config-save) 90))

(define-subwidget (launcher config-contents) (q+:make-qgridlayout)
  (setf (q+:contents-margins config-contents) (values 0 0 0 0)
        (q+:alignment config-contents) (q+:qt.align-top))
  (q+:add-widget config-contents (q+:make-qlabel "Path to Furcadia") 0 0 1 2)
  (q+:add-widget config-contents config-furcadia-path 1 0 1 2)
  (q+:add-widget config-contents (q+:make-qlabel "Accounts") 2 0 1 2)
  (q+:add-widget config-contents config-add-account 3 0)
  (q+:add-widget config-contents config-delete-account 3 1)
  (q+:add-widget config-contents config-accounts-widget 4 0 1 2))

(define-subwidget (launcher config-contents-widget) (q+:make-qwidget)
  (setf (q+:layout config-contents-widget) config-contents))

(define-subwidget (launcher config-scroll) (q+:make-qscrollarea)
  (setf (q+:widget config-scroll) config-contents-widget
        (q+:widget-resizable config-scroll) t
        (q+:frame-shape config-scroll) (q+:qframe.no-frame)))

(define-subwidget (launcher config-layout) (q+:make-qgridlayout)
  (setf (q+:contents-margins config-layout) (values 0 0 0 0))
  (q+:add-widget config-layout config-save 0 0)
  (q+:add-widget config-layout config-scroll 1 0 1 2))

(define-subwidget (launcher config-box) (q+:make-qwidget)
  (setf (q+:layout config-box) config-layout))

(defun new-account-number (widget)
  (let ((accounts (find-children widget "QLineEdit")))
    (1+ (/ (length accounts) 2))))

(defun add-credential-input (widget)
  (let* ((layout (q+:layout widget))
         (n (new-account-number widget))
         (email (q+:make-qlineedit))
         (password (q+:make-qlineedit)))
    (setf (q+:echo-mode password) (q+:qlineedit.password)
          (q+:placeholder-text email) (format nil "Account ~D: email" n)
          (q+:placeholder-text password) (format nil "Account ~D: password" n))
    (q+:add-widget layout email)
    (q+:add-widget layout password)))

(defun remove-credential-input (widget)
  (when (< 1 (new-account-number widget))
    (let ((accounts (find-children widget "QLineEdit")))
      (finalize (last accounts 2))
      (finalize (last accounts 1)))))

(defun get-all-credentials (widget)
  (let ((children (find-children widget "QLineEdit")))
    (mapcar #'q+:text children)))

(define-slot (launcher add-account) ()
  (declare (connected config-add-account (clicked)))
  (add-credential-input config-accounts-widget))

(define-slot (launcher delete-acccount) ()
  (declare (connected config-delete-account (clicked)))
  (remove-credential-input config-accounts-widget))
