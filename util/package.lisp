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
           #:*version*))

(defparameter raptor-launcher/util::*version* "0.5alpha"
  "The version of the Raptor Launcher.") ;; TODO move to constants
