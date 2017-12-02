;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; module.lisp

(in-package :raptor-launcher/protocol)

(define-protocol module
    (:description "The MODULE protocol describes objects representing the ~
Raptor Launcher modules, displayable in the main window of the Raptor Launcher.
Modules are expected to be defined using the DEFINE-RAPTOR-MODULE macro."
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
  (:function layout ((module module)) qlayout)
  "Returns the layout of the module, into which it is possible to embed ~
Qt widgets."
  (:function show-module ((module module)) t)
  "Shows the provided module instance inside its main window."
  (:macro define-raptor-module (name (&rest protocol-classes) &body clauses))
  "Defines a new concrete Raptor Launcher module with name NAME, subclassing ~
PROTOCOL-CLASSES.
\
The possible clauses are:
\(:MAIN-WINDOW LAYOUT-CLASS SLOTS) - required, one permitted. LAYOUT-CLASS ~
is the class of Qt layout to be instantiated and embedded in the Raptor ~
Launcher's main window. SLOTS is a list of all slots in the widget.
\(:SELECTOR TEXT) - required, one permitted. TEXT is the text that will be ~
shown on the button.
\(:BUTTON NAME TEXT) - optional, multiple permitted. NAME is the slot name of ~
the button and TEXT is the text that will be shown on it.
\(:CONSTRUCTOR FUNCTION) - optional, one permitted. Must be a function of zero ~
arguments that will be called after the module instance is constructed.")
