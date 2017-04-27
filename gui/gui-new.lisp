;;;; gui-new.lisp

(in-package :furcadia-launcher-gui)

(defmacro launcher-hide-all ()
  `(mapc #'q+:hide
         (list news-box
               config-box
               character-list
               description-preview
               editor-box
               debug-box
               help-box)))

(define-widget launcher (QMainWindow) ())

(define-subwidget (launcher central-widget) (q+:make-qwidget)
  (setf (q+:central-widget launcher) central-widget))

(define-subwidget (launcher image) (q+:make-qlabel)
  (setf (q+:minimum-size image) (values 150 420)
        (q+:maximum-size image) (values 150 420)))

;;;; BUTTONS ABOVE

(define-subwidget (launcher button-news) (q+:make-qpushbutton "News"))
(define-slot (launcher button-news-pressed) ()
  (declare (connected button-news (clicked)))
  (launcher-hide-all)
  (q+:show news-box))

(define-subwidget (launcher button-config) (q+:make-qpushbutton "Config"))
(define-slot (launcher button-config-pressed) ()
  (declare (connected button-config (clicked)))
  (launcher-hide-all)
  (q+:show config-box))

(define-subwidget (launcher button-chars) (q+:make-qpushbutton "Characters"))
(define-slot (launcher button-chars-pressed) ()
  (declare (connected button-chars (clicked)))
  (launcher-hide-all)
  (q+:show character-list)
  (q+:show description-preview))

(define-subwidget (launcher button-editor) (q+:make-qpushbutton "Editor"))
(define-slot (launcher button-editor-pressed) ()
  (declare (connected button-editor (clicked)))
  (launcher-hide-all)
  (q+:show editor-box))

(define-subwidget (launcher button-debug) (q+:make-qpushbutton "Debug"))
(define-slot (launcher button-debug-pressed) ()
  (declare (connected button-debug (clicked)))
  (launcher-hide-all)
  (q+:show debug-box))

(define-subwidget (launcher button-help) (q+:make-qpushbutton "Help"))
(define-slot (launcher button-help-pressed) ()
  (declare (connected button-help (clicked)))
  (launcher-hide-all)
  (q+:show help-box))

;;;; BUTTONS BELOW

(define-subwidget (launcher button-play) (q+:make-qpushbutton "Play!"))

(define-subwidget (launcher button-sync) (q+:make-qpushbutton "Sync"))

(define-subwidget (launcher button-quit) (q+:make-qpushbutton "Quit"))

;;;; CENTERS

(define-subwidget (launcher news-box) (q+:make-qlabel))

(define-subwidget (launcher config-box) (q+:make-qwidget))

(define-subwidget (launcher character-list) (q+:make-qtablewidget))
(define-subwidget (launcher description-preview) (q+:make-qtextedit))

(define-subwidget (launcher editor-box) (q+:make-qwidget))

(define-subwidget (launcher debug-box) (q+:make-qwidget))

(define-subwidget (launcher help-box) (q+:make-qwidget))

;;;; MAIN LAYOUT

(define-subwidget (launcher layout) (q+:make-qgridlayout)
  (setf (q+:window-title launcher) "Launcher"
        (q+:fixed-size launcher) (values 600 420)
        (q+:layout central-widget) layout)
  (mapc (curry #'apply #'q+:add-widget layout)
        `((,image 0 0 11 1)
          (,news-box 0 1 11 1)
          (,config-box 0 1 11 1)
          (,editor-box 0 1 11 1)
          (,debug-box 0 1 11 1)
          (,help-box 0 1 11 1)
          (,character-list 0 1 7 1)
          (,description-preview 7 1 4 1)
          (,button-news 0 2)
          (,button-config 1 2)
          (,button-chars 2 2)
          (,button-editor 3 2)
          (,button-debug 4 2)
          (,button-help 5 2)
          (,button-play 8 2)
          (,button-sync 9 2)
          (,button-quit 10 2)))
  (launcher-hide-all)
  (q+:show character-list)
  (q+:show description-preview))
