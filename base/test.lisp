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
  (mapcar (curry #'make-dummy-button thing "-") (iota amount)))

(defvar *dummies* '())

(defmacro define-test-dummy (symbol)
  `(progn
     (define-widget ,symbol (QLabel module)
       ((main-window :accessor main-window
                     :initform nil)
        (buttons :reader buttons
                 :initform (make-dummy-buttons (+ 1 (random 5)) ',symbol))
        (selector :accessor selector)))
     (define-qt-constructor (,symbol)
       (setf (selector ,symbol) selector)
       (setf (q+:text ,symbol) (string ',symbol)
             (q+:style-sheet ,symbol)
             (format nil "background: #~6,'0X;" (random #.(expt 16 6)))))
     (define-subwidget (,symbol selector)
         (q+:make-qpushbutton (string ',symbol)))
     (define-slot (,symbol selected) ()
       (declare (connected selector (pressed)))
       (hide-all-modules (main-window ,symbol))
       (show-module ,symbol))
     (pushnew ',symbol *dummies*)))

(define-test-dummy dummy-1)

(define-test-dummy dummy-2)

(define-test-dummy dummy-3)

(define-test-dummy dummy-4)

(define-test-dummy dummy-5)

(define-test-dummy dummy-6)

(define-test-dummy dummy-7)

(define-test-dummy dummy-8)

(define-test-dummy dummy-9)

(define-test-dummy dummy-0)

;;; Test 1

(defun test1 ()
  (let ((*available-modules* (reverse *dummies*)))
    (with-main-window (main-window 'main-window))))
