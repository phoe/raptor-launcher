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
  (:function buttons ((module module)) list)
  "Returns a list of QPushButton instances meant to be shown in the module ~
buttons layout."
  (:function selector ((module module)) qpushbutton)
  "Returns an instance of QPushButton meant to be added to the module selector ~
list."
  (:function config-widget-constructor ((module module)) (or function null))
  "Returns a zero-argument function that can be called to create an instance ~
of this module's configuration widget. In case the module has no such widget,
this function returns NIL instead - a default method implementing this ~
behavior is provided."
  (:function main-window ((module module)) (or widget null))
  "Returns the main window the module is loaded into, or NIL if the module is ~
not loaded."
  (:function layout ((module module)) qlayout)
  "Returns the layout of the module, into which it is possible to embed ~
Qt widgets."
  (:function show-module ((module module)) t)
  "Shows the provided module instance inside its main window."
  (:function post-init-callbacks ((module module)) list)
  "Returns a list of all callbacks that are meant to be called once Raptor ~
Launcher finishes initializing itself and all modules. These callbacks are ~
zero-argument function objects."
  (:macro define-raptor-module (name (&rest protocol-classes) &body clauses))
  "Defines a new concrete Raptor Launcher module with name NAME, subclassing ~
PROTOCOL-CLASSES.
\
The possible CLAUSES are:
\(:MAIN-WINDOW LAYOUT-CLASS SLOTS) - required, one permitted. Describes the ~
main window of the module, instantiated when the module is loaded. ~
LAYOUT-CLASS is the class of Qt layout to be instantiated and embedded in the ~
Raptor Launcher's main window. SLOTS is a list of all slots in the widget.
\(:SELECTOR TEXT) - required, one permitted. Describes the selector button ~
that will be used to select the module from the list of all available modules. ~
TEXT is the text that will be shown on the button.
\(:BUTTON NAME TEXT) - optional, multiple permitted. Describes a button that ~
will be shown whenever this module is loaded and shown in the main window. ~
NAME is the slot name of the button and TEXT is the text that will be shown on ~
it.
\(:CONSTRUCTOR FUNCTION) - optional, one permitted. The body of this clause ~
will be called after the module instance is constructed. All Qt slot values ~
are bound inside this constructor body, as via DEFINE-QT-CONSTRUCTOR.")

(defmethod config-widget-constructor ((object module))
  (declare (ignore object))
  nil)
