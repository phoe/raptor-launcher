;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; fusion.lisp

(in-package :raptor-launcher/protocol)

(define-protocol fusion
    (:description "The FUSION protocol describes Raptor Launcher modules that ~
are meant for modifying and altering FOX5 patch files."
     :tags (:raptor-launcher :module :fusion :fox5)
     :export t)
  (:class fusion (module) ())
  "A fusion object. Each class participating in the protocol must subclass ~
this protocol class."
  (:function load-source (fusion pathname) (values))
  "Reads the patch file from the provided pathname into the fusion module. ~
The file should be considered read-only."
  (:config :fusion :animatep)
  "The configuration value stating if the fusion should animate the objects ~
it displays.")
