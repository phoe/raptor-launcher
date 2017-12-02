;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; logger.lisp

(in-package :raptor-launcher/protocol)

(define-protocol logger
    (:description "The LOGGER protocol describes Raptor Launcher modules that ~
are meant for logging debug and diagnostic information."
     :tags (:raptor-launcher :module :logger)
     :export t)
  (:class logger (module) ())
  "A logger object. Each class participating in the protocol must subclass ~
this protocol class."
  (:function note (type message &rest args) (values))
  "Logs the provided message on all loggers available in the module's main ~
window."
  (:function note-using-class (class type message &rest args) (values))
  "Logs the provided message on the logger of given class available in the ~
module's main window. Signals an error if no such logger is available."
  (:function logs ((logger logger)) t)
  "Returns a list of all messages logged by the logger."
  (:function clear ((logger logger)) (values))
  "Clears all messages logged by the logger.")
