;;;; gui.lisp

(in-package :furcadia-launcher-gui)
(in-readtable :qtools)

(define-widget main-window (QMainWindow) ())

;; (with-main-window (window 'main-window))

;; (with-main-window (window 'main-window)
;;   (let ((list-widget (q+:make-qlistwidget)))
;;     (print list-widget)
;;     (setf (q+:central-widget window) list-widget)
;;     (q+:insert-item list-widget 0 "haha")
;;     (q+:insert-item list-widget 1 "nooo")))
