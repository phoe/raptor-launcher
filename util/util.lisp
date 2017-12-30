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

(defun rgba-argb (vector)
  (let ((result (make-array (length vector)
                            :element-type (array-element-type vector))))
    (loop for i from 0 below (length vector) by 4
          for a = (svref vector (+ 0 i))
          for b = (svref vector (+ 1 i))
          for c = (svref vector (+ 2 i))
          for d = (svref vector (+ 3 i))
          do (setf (svref result (+ 0 i)) d
                   (svref result (+ 1 i)) a
                   (svref result (+ 2 i)) b
                   (svref result (+ 3 i)) c)
          finally (return result))))
