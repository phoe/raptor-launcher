;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; package.lisp

(defpackage #:raptor-launcher/config
  (:use #:cl
        #:alexandria
        #:ubiquitous)
  (:export
   #:*home-path*
   #:*config-path*
   #:*dl-path*
   #:config
   #:default-config
   #:econfig
   #:remconfig
   #:with-config-transaction
   #:config-alist))
