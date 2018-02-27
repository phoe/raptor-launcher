;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; module.lisp

(in-package :raptor-launcher/raptor-fusion)
(in-readtable :qtools)

(define-raptor-module raptor-fusion (fusion)
  (:main-window qwidget qvboxlayout)
  (:selector "Fusion")
  (:priority 600)
  (:button new-button "New")
  (:button open-button "Open")
  (:button save-button "Save")
  (:button source-button "Source")
  (:constructor
      (note t :debug "Raptor Fusion starting.")))

(define-subwidget (raptor-fusion layout) (q+:make-qgridlayout)
  (setf (q+:layout raptor-fusion) layout
        (q+:row-stretch layout 3) 9001
        (q+:contents-margins layout) (values 0 0 0 0)))

(define-subwidget (raptor-fusion destination-label)
    (q+:make-qlabel "<b>DESTINATION:</b> New Fusion.fox")
  (q+:add-widget layout destination-label 0 0 1 2))

(define-subwidget (raptor-fusion destination-objects)
    (q+:make-qscrollarea)
  (q+:add-widget layout destination-objects 1 0 1 2)
  (add-frame-border destination-objects))

(define-subwidget (raptor-fusion destination-count)
    (q+:make-qlabel "Objects: 0")
  (q+:add-widget layout destination-count 2 0 1 2))

(define-subwidget (raptor-fusion selection-description)
    (q+:make-qlabel "No object selected.")
  (q+:add-widget layout selection-description 3 0)
  (add-frame-border selection-description))

(define-subwidget (raptor-fusion selection-shapes)
    (q+:make-qlabel "No object selected.")
  (q+:add-widget layout selection-shapes 3 1)
  (add-frame-border selection-shapes))

(define-subwidget (raptor-fusion source-label)
    (q+:make-qlabel "<b>SOURCE:</b> New Fusion.fox")
  (q+:add-widget layout source-label 4 0 1 2))

(define-subwidget (raptor-fusion source-objects)
    (q+:make-qscrollarea)
  (q+:add-widget layout source-objects 5 0 1 2)
  (add-frame-border source-objects))

(define-subwidget (raptor-fusion source-count)
    (q+:make-qlabel "Objects: 0")
  (q+:add-widget layout source-count 6 0 1 2))

(defun add-frame-border (widget)
  (setf (q+:line-width widget) 2
        (q+:frame-style widget) (logxor (q+:qframe.sunken) (q+:qframe.panel))))
