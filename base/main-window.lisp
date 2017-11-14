;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; package.lisp

(in-package :raptor-launcher/base)
(in-readtable :qtools)

(defvar *loaded-modules* '()
  "The list of all Raptor Launcher modules loaded into the Lisp image, by means
of package designators.
Each module is required to provide a compatible interface.")

(define-widget main-window (QMainWindow) ()
  (:documentation
    "The main window widget for Raptor Launcher."))

(define-widget (main-window central-widget) (q+:make-qwidget)
  (setf (q+:central-widget main-window) central-widget))
