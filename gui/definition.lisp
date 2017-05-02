;;;; definition.lisp

(in-package :furcadia-launcher-gui)
(in-readtable :qtools)

(define-widget launcher (QMainWindow) ())

(define-subwidget (launcher central-widget) (q+:make-qwidget)
  (setf (q+:central-widget launcher) central-widget))
