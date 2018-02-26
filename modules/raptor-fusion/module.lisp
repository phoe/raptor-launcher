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
