;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; editor.lisp

(in-package :raptor-launcher/protocol)

(define-protocol editor
    (:documentation "The EDITOR protocol describes Raptor Launcher modules ~
that are used for viewing and editing Furcadia furres."
     :tags (:raptor-launcher :module :editor)
     :export t)
  (:class editor (module) ())
  "An editor object. Each class participating in the protocol must subclass ~
this protocol class."
  (:function edit-furre ((editor editor) (furre furre)))
  "Selects the provided furre in the editor for editing."
  (:function edit-furre-and-switch ((editor editor) (furre furre)))
  "Selects the furre designator in the editor for editing and selects the ~
editor on the Raptor Launcher to be the active widget.")

(execute-protocol editor)
