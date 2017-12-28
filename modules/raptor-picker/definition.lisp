;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; definition.lisp

(in-package :raptor-launcher/raptor-picker)
(in-readtable :qtools)

(define-raptor-module raptor-picker (picker)
  (:main-window qwidget qhboxlayout
                (cookie-jar :accessor cookie-jar
                            :initform (make-instance 'drakma:cookie-jar)))
  (:selector "Characters")
  (:button play-button "Play!")
  (:button sync-button "Sync")
  (:constructor
      (note t :debug "Raptor Picker starting.")
      (setf (config-widget-constructor raptor-picker)
       (lambda () (make-instance 'config-widget :module raptor-picker)))))

(define-slot (raptor-picker sync) ()
  (declare (connected sync-button (clicked)))
  (sync raptor-picker))
