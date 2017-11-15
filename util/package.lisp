;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; package.lisp

(defpackage #:raptor-launcher/util
  (:use #:cl
        #:alexandria
        #:phoe-toolbox)
  (:export #:keywordize-cars
           #:witty-line
           #:define-qt-constructor
           #:*version*))
