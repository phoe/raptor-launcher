;;;; package.lisp

(defpackage #:furcadia-launcher
  (:use #:cl
        #:alexandria
        #:drakma
        #:json
        #:bordeaux-threads
        #:fare-csv
        #:split-sequence
        #:static-vectors
        #:lparallel.queue)
  (:export #:note
           #:witty-line
           #:*version*
           #:cat))

(defpackage #:furcadia-launcher-gui
  (:use #:cl+qt
        #:alexandria
        #:furcadia-launcher)
  (:export #:main))

(defparameter furcadia-launcher::*version* "0.3.1"
  "The version of the Raptor Launcher.")
