;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; definition.lisp

(in-package :raptor-launcher/raptor-editor)
(in-readtable :qtools)

(define-raptor-module raptor-editor (editor)
  (:main-window qwidget qgridlayout
                (description :accessor description)
                (looks :accessor looks)
                (afk-description :accessor afk-description)
                (afk-looks :accessor afk-looks))
  (:selector "Editor")
  (:constructor
      (mapcar (lambda (class accessor name)
                (funcall (fdefinition `(setf ,accessor))
                         (make-instance class :module raptor-editor :name name)
                         raptor-editor))
              '(description looks description looks)
              '(description looks afk-description afk-looks)
              '("Description" "Looks" "AFK Description" "AFK Looks"))
      (note t :debug "Raptor Editor starting.")))
