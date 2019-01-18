;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; definition.lisp

(in-package :raptor-launcher/raptor-picker)
(in-readtable :qtools)

(define-raptor-module raptor-picker (picker)
  (:main-window qwidget qhboxlayout
                (%loading-screen :accessor loading-screen)
                (%petri-net :accessor petri-net-of
                  :initform nil))
  (:selector "Launcher")
  (:priority 100)
  (:button play-button "Play!")
  (:button sync-button "Sync")
  (:constructor
      (note t :debug "Raptor Picker starting.")
      (setf (config-widget-constructor raptor-picker)
       (lambda () (make-instance 'config-widget :module raptor-picker))
       (loading-screen raptor-picker)
       (make-instance 'loading-screen :module raptor-picker))
      (q+:add-widget (slot-value raptor-picker 'layout)
                     (loading-screen raptor-picker))
      (q+:hide (loading-screen raptor-picker))))

(define-slot (raptor-picker sync) ()
  (declare (connected sync-button (clicked)))
  (sync raptor-picker))
