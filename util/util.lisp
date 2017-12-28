;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; phoe-toolbox.lisp

(in-package :raptor-launcher/util)

(defparameter *version* "0.5alpha"
  "The version of the Raptor Launcher.") ;; TODO move to constants

;;; Common utilities depend on the PHOE-TOOLBOX system.

(defmacro define-qt-constructor ((class . keys) &body body)
  `(define-constructor (,class ,@keys)
     (qtools:with-slots-bound (,class ,class)
       ,@body)))
