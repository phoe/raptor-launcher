;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; package.lisp

(defpackage #:raptor-launcher/protocol
  (:use #:common-lisp
        #:closer-mop
        #:protest)
  (:shadowing-import-from
   #:closer-mop
   #:standard-generic-function
   #:defmethod
   #:defgeneric))
