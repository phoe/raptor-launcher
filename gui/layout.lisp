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
  (setf (q+:window-title launcher) "Raptor Launcher"
        (q+:fixed-size launcher) (values 600 420)
        (q+:layout central-widget) layout)
  (set-launcher-layout)
  (launcher-hide-all)
  (disable-buttons)
  (q+:show chars-box)
  (q+:set-focus character-list))
