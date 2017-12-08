;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; characters.lisp

(in-package :raptor-launcher/protocol)

(define-protocol picker
    (:description "The PICKER protocol describes Raptor Launcher modules that ~
are used for selecting, viewing and launching individual Furcadia furres."
     :tags (:raptor-launcher :module :picker)
     :export t)
  (:class picker (module) ())
  "A picker object. Each class participating in the protocol must subclass ~
this protocol class."
  (:function sync ((picker picker)) t)
  "Signals the picker to asynchronously synchronize with the external servers ~
and returns the list of characters available in the picker. ~
An :AROUND method on this method is provided which updates the furre lists on ~
all EDITOR modules available in the picker's main window by calling the (SETF ~
FURRES) function."
  (:function shortnamename-furre
             ((picker picker) (shortname string)) (furre furre))
  "Returns a furre object matching the provided shortname."
  (:function furres ((picker picker)) t)
  "Returns a list of all furres available in the picker."
  (:function (setf furres) (new-value (picker picker)) new-value)
  "Sets a list of all furres available in the picker."
  (:function select-furre
             ((picker picker) (furre-designator (or furre string)))
             (values))
  "Selects the provided furre in the picker. The furre designator can be a ~
furre object or a shortname. Signals an error if the provided character cannot ~
be found."
  (:function select-character-and-switch
             ((picker picker) (furre-designator (or furre string))) (values))
  "Selects the provided furre in the picker and selects the picker on the ~
module's main window to be the active widget. The furre designator can be a ~
furre object or a shortname. Signals an error if the provided character cannot ~
be found.")

;; TODO create protoclass EDITOR, else this will explode
(defmethod sync :around ((picker picker))
  (let* ((value (call-next-method))
         (modules (loaded-modules *main-window*))
         (editors (remove-if-not (rcurry #'subclassp 'editor) modules
                                 :key #'class-of)))
    (mapc (curry #'(setf furres) value) editors)
    value))
