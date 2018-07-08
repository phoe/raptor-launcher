;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; palette.lisp

(in-package :raptor-launcher/raptor-editor)
(in-readtable :qtools)

(protest:define-protocol palette
    (:documentation "The PALETTE protocol describes widgets that are used ~
inside Raptor Editor. They are used for displaying options for customizing ~
furres, trom which a person may choose."
     :tags (:raptor-launcher :editor :raptor-editor :palette)
     :export nil)
  (:class palette () ())
  "A palette class. Each class participating in the protocol must subclass ~
this protocol class."
  (:function selector ((palette palette)) t)
  "Returns an instance of QToolButton meant to be added to the palette ~
selector list.")

(protest:execute-protocol palette)
