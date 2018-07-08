;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; picker.lisp

(in-package :raptor-launcher/protocol)

(define-protocol picker
    (:documentation "The PICKER protocol describes Raptor Launcher modules ~
that are used for selecting, viewing and launching individual Furcadia furres."
     :tags (:raptor-launcher :module :picker)
     :export t)
  (:class picker (module) ())
  "A picker object. Each class p!articipating in the protocol must subclass ~
this protocol class."
  (:function sync ((picker picker)) list)
  "Signals the picker to asynchronously synchronize with the external servers."
  (:function shortnamename-furre
             ((picker picker) (shortname string)) (furre furre))
  "Returns a furre object matching the provided shortname."
  (:function furres ((picker picker)) t)
  "Returns a list of all furres available in the picker."
  (:function select-furre
             ((picker picker) (furre-designator (or furre string)))
             (values))
  "Selects the provided furre in the picker. The furre designator can be a ~
furre object or a shortname. Signals an error if the provided furre cannot be ~
found."
  (:function select-character-and-switch
             ((picker picker) (furre-designator (or furre string))) (values))
  "Selects the provided furre in the picker and selects the picker on the ~
Raptor Launcher to be the active widget. The furre designator can be a furre ~
object or a shortname. Signals an error if the provided furre cannot be found."
  (:config (:picker :show-account-number))
  "The configuration value stating if the character list should display ~
account numbers for each displayed furre."
  (:config (:picker :show-last-login))
  "The configuration value stating if the character list should display ~
last login dates for each displayed furre.")

(execute-protocol picker)
