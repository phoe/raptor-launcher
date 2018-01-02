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
          for a = (aref vector (+ 0 i))
          for b = (aref vector (+ 1 i))
          for c = (aref vector (+ 2 i))
          for d = (aref vector (+ 3 i))
          do (setf (aref result (+ 0 i)) d
                   (aref result (+ 1 i)) a
                   (aref result (+ 2 i)) b
                   (aref result (+ 3 i)) c)
          finally (return result))))
