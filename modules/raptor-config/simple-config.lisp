;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; simple-config.lisp

(in-package :raptor-launcher/raptor-config)
(in-readtable :qtools)

(define-subwidget (raptor-config simple-config) (q+:make-qwidget)
  (q+:add-tab config-tabs simple-config "Simple"))

(define-subwidget (raptor-config scroll-layout) (q+:make-qgridlayout)
  (setf (q+:layout simple-config) scroll-layout
        (q+:contents-margins scroll-layout) (values 0 0 0 0)))

(define-subwidget (raptor-config scroll) (q+:make-qscrollarea)
  (q+:add-widget scroll-layout scroll 0 0 1 1)
  (setf (q+:widget-resizable scroll) t
        (q+:frame-shape scroll) (q+:qframe.no-frame)))

(define-subwidget (raptor-config contents) (q+:make-qwidget)
  (setf (q+:widget scroll) contents))

(define-subwidget (raptor-config simple-config-layout) (q+:make-qvboxlayout)
  (setf (q+:layout contents) simple-config-layout
        (q+:contents-margins simple-config-layout) (values 0 0 0 0)))
