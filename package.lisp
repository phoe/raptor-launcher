;;;; package.lisp

(defpackage #:furcadia-launcher
  (:use #:cl
        #:alexandria
        #:drakma
        #:json
        #:bordeaux-threads
        #:lparallel.queue))

(defpackage #:furcadia-launcher-gui
  (:use #:cl+qt
        #:furcadia-launcher)
  (:export #:main))
