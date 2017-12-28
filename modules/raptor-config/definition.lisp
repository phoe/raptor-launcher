;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; definition.lisp

(in-package :raptor-launcher/raptor-config)
(in-readtable :qtools)

(define-raptor-module raptor-config (config)
  (:main-window qwidget qhboxlayout
                (log-list :accessor log-list :initform '()))
  (:selector "Config")
  (:constructor
      (note t :debug "Raptor Config starting.")
      (setf (config-widget-constructor raptor-config)
       (lambda () (make-instance 'config-widget :module raptor-config)))
      (push (make-spacer raptor-config)
            (post-init-callbacks raptor-config))
      (push (make-callback raptor-config)
            (post-init-callbacks raptor-config))))

(define-subwidget (raptor-config config-tabs) (q+:make-qtabwidget)
  (q+:add-widget layout config-tabs))
