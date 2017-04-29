;;;; package.lisp

(defpackage #:furcadia-launcher
  (:use #:cl
        #:alexandria
        #:drakma
        #:json
        #:bordeaux-threads
        #:fare-csv
        #:split-sequence
        #:lparallel.queue))

(defpackage #:furcadia-launcher-gui
  (:use #:cl+qt
        #:alexandria
        #:furcadia-launcher)
  (:export #:build-main))
