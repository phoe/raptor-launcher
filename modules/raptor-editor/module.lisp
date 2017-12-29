;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; module.lisp

(in-package :raptor-launcher/raptor-editor)
(in-readtable :qtools)

(define-raptor-module raptor-editor (editor)
  (:main-window qwidget qgridlayout)
  (:selector "Editor")
  (:constructor
      (make-instance 'description :module raptor-editor)
      (make-instance 'looks :module raptor-editor)
      (make-instance 'afk-looks :module raptor-editor)
      (note t :debug "Raptor Editor starting.")))

;;; Main layout

(define-subwidget (raptor-editor preview) (q+:make-qwidget)
  (q+:add-widget layout preview 0 0 5 1)
  (setf (q+:fixed-size preview) (values 135 135)
        (q+:style-sheet preview) "background-color:black;")
  (let ((size-policy (q+:size-policy preview)))
    (setf (q+:horizontal-policy size-policy) (q+:qsizepolicy.fixed)
          (q+:vertical-policy size-policy) (q+:qsizepolicy.fixed))))

(define-subwidget (raptor-editor dropdown) (q+:make-qcombobox)
  (q+:add-widget layout dropdown 1 1 1 2))

(define-subwidget (raptor-editor save-button)
    (q+:make-qpushbutton "Save to server")
  (q+:add-widget layout save-button 2 1 1 1))

(define-subwidget (raptor-editor restore-button)
    (q+:make-qpushbutton "Restore from server")
  (q+:add-widget layout restore-button 2 2 1 1))

(define-subwidget (raptor-editor separator) (q+:make-qframe)
  (q+:add-widget layout separator 3 1 1 2)
  (setf (q+:frame-shape separator) (q+:qframe.hline)
        (q+:frame-shadow separator) (q+:qframe.sunken)))

(define-subwidget (raptor-editor spacing) (q+:make-qwidget)
  (q+:add-widget layout spacing 3 1 1 2))

(define-subwidget (raptor-editor misc-layout) (q+:make-qhboxlayout)
  (q+:add-layout layout misc-layout 4 1 1 2))

(define-subwidget (raptor-editor costume-dropdown) (q+:make-qcombobox)
  (q+:add-widget misc-layout costume-dropdown 9001))

(define-subwidget (raptor-editor add-costume-button)
    (q+:make-qtoolbutton)
  (q+:add-widget misc-layout add-costume-button 1)
  (setf (q+:text add-costume-button) "New"
        (q+:tool-button-style add-costume-button)
        (q+:qt.tool-button-text-only)))

(define-subwidget (raptor-editor delete-costume-button)
    (q+:make-qtoolbutton)
  (q+:add-widget misc-layout delete-costume-button 1)
  (setf (q+:text delete-costume-button) "Delete"
        (q+:tool-button-style delete-costume-button)
        (q+:qt.tool-button-text-only)))

(define-subwidget (raptor-editor rating-dropdown) (q+:make-qcombobox)
  (q+:add-widget misc-layout rating-dropdown 1)
  (setf (q+:minimum-contents-length rating-dropdown) 4)
  (q+:add-items rating-dropdown '("T+" "M16+" "AOC" "A18+" "AO")))

(define-subwidget (raptor-editor tabs) (q+:make-qtabwidget)
  (q+:add-widget layout tabs 5 0 1 3))

;;; Tabs

(define-widget description (qwidget)
  ((module :accessor module
           :initarg :module)))

(define-qt-constructor (description)
  (with-slots-bound (module raptor-editor)
    (q+:add-tab tabs description "Description")))

(define-widget looks (qwidget)
  ((module :accessor module
           :initarg :module)))

(define-qt-constructor (looks)
  (with-slots-bound (module raptor-editor)
    (q+:add-tab tabs looks "Looks")))

(define-widget afk-looks (qwidget)
  ((module :accessor module
           :initarg :module)))

(define-qt-constructor (afk-looks)
  (with-slots-bound (module raptor-editor)
    (q+:add-tab tabs afk-looks "AFK Looks")))
