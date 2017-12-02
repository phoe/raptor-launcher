;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; package.lisp

(uiop:define-package #:raptor-launcher/protocol
  (:use #:cl
        #:closer-mop
        #:moptilities
        #:alexandria
        #:protest)
  (:shadowing-import-from
   #:closer-mop
   #:standard-generic-function
   #:defmethod
   #:defgeneric))
