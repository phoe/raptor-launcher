;;;; gui-new.lisp

(in-package :furcadia-launcher-gui)

(define-widget launcher (QMainWindow) ())

(define-subwidget (launcher central-widget) (q+:make-qwidget)
  (setf (q+:central-widget launcher) central-widget))

(define-subwidget (launcher image) (q+:make-qlabel)
  (setf (q+:minimum-size image) (values 150 420)
        (q+:maximum-size image) (values 150 420)))

(define-subwidget (launcher character-list) (q+:make-qtablewidget)
  ;; (setf (q+:minimum-width character-list) 320
  ;;       (q+:maximum-width character-list) 320)
  )

(define-subwidget (launcher description-preview) (q+:make-qtextedit))

(define-subwidget (launcher button-play) (q+:make-qpushbutton "Play!"))

(define-subwidget (launcher button-sync) (q+:make-qpushbutton "Sync"))

(define-subwidget (launcher button-news) (q+:make-qpushbutton "News"))

(define-subwidget (launcher button-accounts) (q+:make-qpushbutton "Accounts"))

(define-subwidget (launcher button-chars) (q+:make-qpushbutton "Characters"))

(define-subwidget (launcher button-editor) (q+:make-qpushbutton "Editor"))

(define-subwidget (launcher button-debug) (q+:make-qpushbutton "Debug"))

(define-subwidget (launcher button-help) (q+:make-qpushbutton "Help"))

(define-subwidget (launcher button-quit) (q+:make-qpushbutton "Quit"))

(define-subwidget (launcher layout) (q+:make-qgridlayout)
  (setf (q+:window-title launcher) "Launcher"
        (q+:fixed-size launcher) (values 600 420)
        (q+:layout central-widget) layout)
  (mapc (curry #'apply #'q+:add-widget layout)
        `((,image 0 0 10 1)
          (,character-list 0 1 6 1)
          (,description-preview 6 1 4 1)
          (,button-play 1 2)
          (,button-sync 2 2)
          (,button-news 3 2)
          (,button-accounts 4 2)
          (,button-chars 5 2)
          (,button-editor 6 2)
          (,button-debug 7 2)
          (,button-help 8 2)
          (,button-quit 9 2))))
