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
  (:function note (logger type message &rest args) t)
  "Logs the provided message on the provided LOGGER. MESSAGE and ARGS should ~
follow the same convention as FORMAT arguments.
\
The user might provide T as a logger, at which point, the message will be sent ~
to all loggers available in the *MAIN-WINDOW*. The user may also ~
provide NIL, at which point the function will simply return a two-element list ~
containing TYPE and the formatted message."
  (:function logs ((logger logger) &key severity) t)
  "Returns a list of all messages logged by the logger of equal or higher ~
importance than the provided one."
  (:function clear ((logger logger)) (values))
  "Clears all messages logged by the logger."
  (:variable *message-types* t
             '((:trace 191 191 191) (:debug 000 255 255) (:info 000 255 000)
               (:warn 255 255 000) (:error 255 000 000)
               (:severe 255 000 127) (:fatal 255 000 255)))
  "A list of message types valid for logging, in increasing order of severity. ~
The first element of each type is the type's keyword, the second is a list of ~
default RGB values for displaying it."
  (:config (:logger))
  "The root node for all logger configuration."
  (:config (:logger :message-type :color type))
  "The configuration value describing the color for the provided TYPE, which ~
is a message type. The value of this should be a list of three integers, ~
denoting a RGB color.
\
This configuration value is expected to be defined for all types mentioned in ~
*MESSAGE-TYPES*, with the default values are to be taken from the list.")

(defmethod note ((logger (eql 't)) type message &rest args)
  (when *main-window*
    (let* ((modules (loaded-modules *main-window*))
           (loggers (remove-if-not (rcurry #'subclassp 'logger) modules
                                   :key #'class-of)))
      (mapc (lambda (x) (apply #'note x type message args)) loggers)
      (values))))

(defmethod note ((logger (eql 'nil)) type message &rest args)
  (list type (apply #'format nil message args)))
