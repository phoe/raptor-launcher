;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; package.lisp

(defpackage #:raptor-launcher/util
  (:use #:cl
        #:alexandria
        #:phoe-toolbox)
  (:export
   #:witty-line
   #:witty-password
   #:rgba-argb
   #:keywordize-cars
   #:define-qt-constructor
   #:*version*))
