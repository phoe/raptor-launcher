;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; layout.lisp

(in-package :raptor-launcher/raptor-editor)
(in-readtable :qtools)

(define-subwidget (raptor-editor preview) (q+:make-qwidget)
  (q+:add-widget layout preview 0 0 5 1)
  (setf (q+:fixed-size preview) (values 135 135))
  (let ((size-policy (q+:size-policy preview)))
    (setf (q+:horizontal-policy size-policy) (q+:qsizepolicy.fixed)
          (q+:vertical-policy size-policy) (q+:qsizepolicy.fixed))))

(define-subwidget (raptor-editor dropdown) (q+:make-qcombobox)
  (q+:add-widget layout dropdown 0 1 1 2))

(define-subwidget (raptor-editor save-button)
    (q+:make-qpushbutton "Save to server")
  (q+:add-widget layout save-button 1 1 1 1))

(define-subwidget (raptor-editor restore-button)
    (q+:make-qpushbutton "Restore from server")
  (q+:add-widget layout restore-button 1 2 1 1))

(define-subwidget (raptor-editor separator) (q+:make-qframe)
  (q+:add-widget layout separator 2 1 1 2)
  (setf (q+:frame-shape separator) (q+:qframe.hline)
        (q+:frame-shadow separator) (q+:qframe.sunken)))

(define-subwidget (raptor-editor misc-layout) (q+:make-qhboxlayout)
  (q+:add-layout layout misc-layout 3 1 1 2))

(define-subwidget (raptor-editor costume-dropdown) (q+:make-qcombobox)
  (q+:add-widget misc-layout costume-dropdown 9001))

(define-subwidget (raptor-editor add-costume-button)
    (make-text-qtoolbutton "+")
  (q+:add-widget misc-layout add-costume-button 1))

(define-subwidget (raptor-editor delete-costume-button)
    (make-text-qtoolbutton "-")
  (q+:add-widget misc-layout delete-costume-button 1))

(define-subwidget (raptor-editor rating-dropdown) (q+:make-qcombobox)
  (q+:add-widget misc-layout rating-dropdown 1)
  (setf (q+:minimum-contents-length rating-dropdown) 4)
  (q+:add-items rating-dropdown '("T+" "M16+" "AOC" "A18+" "AO")))

(define-subwidget (raptor-editor tabs) (q+:make-qtabwidget)
  (q+:add-widget layout tabs 5 0 1 3))
