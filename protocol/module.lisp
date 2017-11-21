;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; module.lisp

(in-package :raptor-launcher/protocol)

(define-protocol module
    (:description "The MODULE protocol describes objects representing the ~
Raptor Launcher modules, displayable in the main window of the Raptor Launcher."
     :tags (:raptor-launcher :module)
     :export t)
  (:class module () ())
  "A module object. Each class participating in the protocol must subclass ~
this protocol class."
  (:function buttons ((module module)) t)
  "Returns a list of QPushButton instances meant to be shown in the module ~
buttons layout."
  (:function selector ((module module)) qpushbutton)
  "Returns an instance of QPushButton meant to be added to the module selector ~
list."
  (:function main-window ((module module)) (or widget null))
  "Returns the main window the module is loaded into, or NIL if the module is ~
not loaded."
  (:function show-module ((module module)) t)
  "Shows the provided module instance inside its main window.")
