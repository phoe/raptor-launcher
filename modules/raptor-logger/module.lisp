(defpackage :raptor-launcher/raptor-logger
  (:use #:cl+qt
        #:alexandria
        #:phoe-toolbox
        #:raptor-launcher/util
        #:raptor-launcher/protocol
        #:raptor-launcher/base)
  (:export #:raptor-logger))

(in-package :raptor-launcher/raptor-logger)
(in-readtable :qtools)

(define-raptor-module raptor-logger (logger)
  (:main-window qhboxlayout)
  (:selector "Logger")
  (:button clear-button "Clear"))
