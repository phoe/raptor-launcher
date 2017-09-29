;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; package.lisp

(defpackage #:raptor-launcher
  (:use #:cl
        #:alexandria
        #:phoe-toolbox
        #|
        #:drakma
        #:json
        #:bordeaux-threads
        #:fare-csv
        #:split-sequence
        #:static-vectors
        #:lparallel.queue
        )
  (:export #:note
           #:witty-line
           #:*version*
           #:cat)
        |#))
#|
(defpackage #:furcadia-launcher-gui
  (:use #:cl+qt
        #:alexandria
        #:furcadia-launcher)
  (:export #:main))
|#

(defparameter raptor-launcher::*version* "0.4alpha"
  "The version of the Raptor Launcher.") ;; TODO move to constants
