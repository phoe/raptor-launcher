;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; package.lisp

(defpackage #:raptor-launcher/config
  (:use #:cl
        #:ubiquitous)
  (:export
   #:*home-path*
   #:*config-path*
   #:*dl-path*
   #:config
   #:remconfig))
