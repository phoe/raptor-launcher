;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; definition.lisp

(in-package :raptor-launcher/raptor-chat)
(in-readtable :qtools)

(define-raptor-module raptor-chat (module #|chat|#)
  (:main-window qwidget qhboxlayout)
  (:selector "Chat")
  (:priority 750)
  (:constructor
      (note t :debug "Raptor Chat starting.")))
