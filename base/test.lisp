;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; test.lisp

(defpackage :raptor-launcher/test
  (:use #:cl+qt
        #:alexandria
        #:phoe-toolbox
        #:raptor-launcher/util
        #:raptor-launcher/base)
  (:export #:dummy-1 #:dummy-2 #:dummy-3))

(in-package :raptor-launcher/test)
(in-readtable :qtools)

;;; util

(defun make-dummy-button (&rest things)
  (q+:make-qpushbutton (format nil "~{~A~^ ~}" things)))

(defun make-dummy-buttons (amount thing)
  (mapcar (curry #'make-dummy-button thing) (iota amount)))

;;; dummy-1

(define-widget dummy-1 (QLabel module)
  ((buttons :reader buttons
            :initform (make-dummy-buttons (+ 5 (random 5)) 'dummy-1))
   (main-window :accessor main-window
                :initform nil)
   (module-selector :accessor module-selector)))

(define-subwidget (dummy-1 selector)
    (q+:make-qpushbutton (cat "DUMMY-1 " (prin1-to-string (random 1000)))))

(define-slot (dummy-1 selected) ()
  (declare (connected selector (pressed)))
  (hide-all-modules (main-window dummy-1))
  (show-module dummy-1))

(define-qt-constructor (dummy-1)
  (setf (q+:text dummy-1) (cat "DUMMY-1 " (prin1-to-string (random 1000))))
  (setf (module-selector dummy-1) selector
        (q+:style-sheet dummy-1) "background: blue;"))

;;; dummy-2

(define-widget dummy-2 (QLabel module)
  ((buttons :reader buttons
            :initform (list (make-dummy-button :dummy-2 :a (random 1000))
                            (make-dummy-button :dummy-2 :b (random 1000))
                            (make-dummy-button :dummy-2 :c (random 1000))))
   (main-window :accessor main-window
                :initform nil)
   (module-selector :accessor module-selector)))

(define-subwidget (dummy-2 selector)
    (q+:make-qpushbutton (cat "DUMMY-2 " (prin1-to-string (random 1000)))))

(define-slot (dummy-2 selected) ()
  (declare (connected selector (pressed)))
  (hide-all-modules (main-window dummy-2))
  (show-module dummy-2))

(define-qt-constructor (dummy-2)
  (setf (q+:text dummy-2) (cat "DUMMY-2 " (prin1-to-string (random 1000))))
  (setf (module-selector dummy-2) selector
        (q+:style-sheet dummy-2) "background: #FFFF00;"))

;;; Test 1

(defun test1 ()
  (let ((*available-modules* '(dummy-1 dummy-2)))
    (with-main-window (main-window 'main-window))))
