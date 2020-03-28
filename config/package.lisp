;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; package.lisp

(uiop:define-package #:raptor-launcher/config
  (:mix #:cl
        #:alexandria
        #:phoe-toolbox
        #:cl-furcadia/constants
        #:cl-furcadia/protocol
        #:cl-furcadia/clos
        #:ubiquitous
        #:raptor-launcher/protocol)
  (:export
   #:*home-path*
   #:*digo-path*
   #:*config-path*
   #:*dl-path*
   #:config
   #:default-config
   #:econfig
   #:remconfig
   #:with-config-transaction
   #:config-alist
   #:store-object
   #:restore-object
   #:restore-all-objects
   #:postprocess))
