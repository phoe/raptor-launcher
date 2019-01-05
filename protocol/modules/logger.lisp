;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; logger.lisp

(in-package :raptor-launcher/protocol)

(define-protocol logger
    (:documentation "The LOGGER protocol describes Raptor Launcher modules ~
that are meant for logging debug and diagnostic information."
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
  (:variable *log-levels* t
             '((:trace 191 191 191) (:debug 000 255 255) (:info 000 255 000)
               (:warn 255 255 000) (:error 255 000 000)
               (:severe 255 000 127) (:fatal 255 000 255)))
  "A list of log levels valid for logging, in increasing order of severity. ~
The first element of each type is the type's keyword, the second is a list of ~
default RGB values for displaying it."
  (:config (:logger))
  "The root node for all logger configuration."
  (:config (:logger :log-level :min-level-shown))
  "The configuration value describing the minimum logging level shown in the ~
logger. The value of this should be any type mentioned in *LOG-LEVELS*."
  (:config (:logger :log-level :color level))
  "The configuration value describing the color for the provided LEVEL, which ~
is a valid log level. The value of this should be a list of three integers, ~
denoting a RGB color.
\
This configuration value is expected to be defined for all types mentioned in ~
*LOG-LEVELS*, with the default values are to be taken from the list.")

(execute-protocol logger)

(defmethod note ((logger (eql 't)) (type symbol) message &rest args)
  (cond (*main-window*
         (let* ((modules (loaded-modules *main-window*))
                (loggers (remove-if-not (rcurry #'subclassp 'logger) modules
                                        :key #'class-of)))
           (cond
             (loggers
              (mapc (lambda (x) (apply #'note x type message args)) loggers))
             (t (apply #'format t message args) (terpri)))))
        (t (apply #'format t message args) (terpri)))
  (values))

(defmethod note ((logger (eql 'nil)) (type symbol) message &rest args)
  (list type (apply #'format nil message args)))
