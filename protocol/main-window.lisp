;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; main-window.lisp

(in-package :raptor-launcher/protocol)

(define-protocol main-window
    (:description "The MAIN-WINDOW protocol describes main windows of the ~
Raptor Launcher, each capable of loading and displaying modules."
     :tags (:raptor-launcher :main-window)
     :export t)
  (:class main-window () ())
  "A main window object."
  (:variable *main-window* t '())
  "The currently active main window. This variable must be bound whenever the ~
main QApplication is executed."
  (:function loaded-modules ((main-window main-window)) t)
  "Returns a list of all modules loaded into the main window. The modules are ~
indirect instances of the MODULE protocol class."
  (:variable *available-modules* t '())
  "A dynamic variable holding a list of all available modules that are ~
loadable into a main window."
  (:function instantiate-modules
             ((main-window main-window) &optional modules) t)
  "Creates instances of all modules in the modules list and adds them to the ~
list of modules loaded in the provided main window."
  (:function load-modules
             ((main-window main-window) &optional instances) t)
  "Loads all modules from the provided list of instances into the provided main
window."
  (:function hide-all-modules ((main-window main-window)) t)
  "Hides all modules inside the provided main window.")
