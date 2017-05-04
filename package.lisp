;;;; package.lisp

(defpackage #:furcadia-launcher
  (:use #:cl
        #:alexandria
        #:drakma
        #:json
        #:bordeaux-threads
        #:fare-csv
        #:split-sequence
        #:lparallel.queue)
  (:export #:note
           #:witty-line
           #:*version*))

(defpackage #:furcadia-launcher-gui
  (:use #:cl+qt
        #:alexandria
        #:furcadia-launcher)
  (:export #:main))

(defvar furcadia-launcher::*version* "0.2pre"
  "The version of the Raptor Launcher.")
