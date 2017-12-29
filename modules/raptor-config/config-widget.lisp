;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; config-widget.lisp

(in-package :raptor-launcher/raptor-config)
(in-readtable :qtools)

;; TODO config-widget protoclass...?
(define-widget config-widget (qwidget)
  ((module :accessor module
           :initarg :module)))

(define-subwidget (config-widget layout) (q+:make-qgridlayout)
  (setf (q+:layout config-widget) layout))

(define-subwidget (config-widget title) (q+:make-qlabel "Configuration")
  (q+:add-widget layout title 0 0 1 1)
  (setf (q+:style-sheet title) "font-weight: bold;"))

(define-subwidget (config-widget separator) (q+:make-qframe)
  (q+:add-widget layout separator 1 0 1 1)
  (setf (q+:frame-shape separator) (q+:qframe.hline)
        (q+:frame-shadow separator) (q+:qframe.sunken)))

(define-subwidget (config-widget checkbox)
    (q+:make-qcheckbox "Show advanced options?")
  (q+:add-widget layout checkbox 2 0 1 1)
  (let* ((checkedp (config :config :show-advanced))
         (state (if checkedp (q+:qt.checked) (q+:qt.unchecked))))
    (setf (q+:check-state checkbox) state)))

(define-slot (config-widget show-advanced) ()
  (declare (connected checkbox (clicked)))
  (let ((checkedp (q+:is-checked checkbox)))
    (setf (config :config :show-advanced) checkedp)
    (signal! (module config-widget)
             (advanced-checkbox-clicked bool) checkedp)))

(define-qt-constructor (config-widget)
  (let ((checkedp (q+:is-checked checkbox)))
    (signal! (module config-widget) (advanced-checkbox-clicked bool) checkedp)))
