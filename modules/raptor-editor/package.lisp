;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; package.lisp

(defpackage :raptor-launcher/raptor-editor
  (:use #:cl+qt
        #:alexandria
        #:phoe-toolbox
        #:static-vectors
        #:cl-furcadia/constants
        #:raptor-launcher/util
        #:raptor-launcher/protocol
        #:raptor-launcher/config
        #:raptor-launcher/base)
  (:shadowing-import-from #:phoe-toolbox #:split)
  (:export #:raptor-editor))
