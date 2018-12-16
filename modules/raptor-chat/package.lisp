;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2018
;;;; package.lisp

(defpackage #:raptor-launcher/raptor-chat
  (:use #:cl+qt
        #:alexandria
        #:phoe-toolbox
        #:split-sequence
        #:raptor-launcher/util
        #:raptor-launcher/protocol
        #:raptor-launcher/config
        #:raptor-launcher/base)
  (:shadowing-import-from #:phoe-toolbox #:split)
  (:export #:raptor-chat))
