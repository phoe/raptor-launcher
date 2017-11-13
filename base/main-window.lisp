;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; package.lisp

(in-package :raptor-launcher/base)
(in-readtable :qtools)

(define-widget main-window (QMainWindow) ())

(define-widget (main-window central-widget) (q+:make-qwidget)
  (setf (q+:central-widget main-window) central-widget))
