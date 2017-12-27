;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; config.lisp

(in-package :raptor-launcher/protocol)

(define-protocol config
    (:description "The CONFIG protocol describes Raptor Launcher modules that ~
are meant for allowing the user to read and modify the Launcher's configuration."
     :tags (:raptor-launcher :module :config)
     :export t)
  (:class config (module) ())
  "A config object. Each class participating in the protocol must subclass ~
this protocol class.")
