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
      (make-instance 'afk-description :module raptor-editor)
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

;;; Tabs

;;; Looks tab

(define-widget looks (qwidget)
  ((module :accessor module
           :initarg :module)))

(define-qt-constructor (looks)
  (with-slots-bound (module raptor-editor)
    (q+:add-tab tabs looks "Looks")))










































;;; Description tab

(define-widget description (qwidget)
  ((module :accessor module
           :initarg :module)
   (ntags :accessor ntags
          :initform 0)))

(define-qt-constructor (description)
  (with-slots-bound (module raptor-editor)
    (q+:add-tab tabs description "Description")))

(define-subwidget (description scroll-layout) (q+:make-qgridlayout)
  (setf (q+:layout description) scroll-layout
        (q+:contents-margins scroll-layout) (values 0 0 0 0)))

(define-subwidget (description scroll) (q+:make-qscrollarea)
  (q+:add-widget scroll-layout scroll 0 0 1 1)
  (setf (q+:widget-resizable scroll) t
        (q+:frame-shape scroll) (q+:qframe.no-frame)))

(define-subwidget (description contents) (q+:make-qwidget)
  (setf (q+:widget scroll) contents))

(define-subwidget (description layout) (q+:make-qvboxlayout)
  (setf (q+:layout contents) layout))

;;; TODO we need a QScrollArea

;;; Description editor

(define-subwidget (description desc-layout) (q+:make-qgridlayout)
  (q+:add-layout layout desc-layout)
  (setf (q+:column-stretch desc-layout 2) 9001))

(define-subwidget (description desc-title)
    (q+:make-qlabel "Description")
  (q+:add-widget desc-layout desc-title 0 0 1 4)
  (setf (q+:style-sheet desc-title) "font-weight: bold;"))

(define-subwidget (description desc-separator) (q+:make-qframe)
  (q+:add-widget desc-layout desc-separator 1 0 1 4)
  (setf (q+:frame-shape desc-separator) (q+:qframe.hline)
        (q+:frame-shadow desc-separator) (q+:qframe.sunken)))

(define-subwidget (description classic-desc-button)
    (q+:make-qpushbutton "Classic")
  (q+:add-widget desc-layout classic-desc-button 2 0 1 1))

(define-subwidget (description organized-desc-button)
    (q+:make-qpushbutton "Organized")
  (q+:add-widget desc-layout organized-desc-button 2 1 1 1))

(define-subwidget (description preview-desc-button)
    (q+:make-qpushbutton "Preview")
  (q+:add-widget desc-layout preview-desc-button 2 3 1 1))

(define-subwidget (description editor) (q+:make-qplaintextedit)
  (q+:add-widget desc-layout editor 3 0 1 4)
  (with-finalizing ((metrics (q+:make-qfontmetrics (q+:font editor))))
    (let ((row-height (q+:line-spacing metrics)))
      (setf (q+:minimum-height editor) (* 10 row-height)
            (q+:maximum-height editor) (* 10 row-height)))))

;;; Tag editor

(define-subwidget (description tag-layout) (q+:make-qgridlayout)
  (q+:add-layout layout tag-layout)
  (setf (q+:column-stretch tag-layout 3) 9001))

(define-subwidget (description tag-title)
    (q+:make-qlabel "Tags")
  (q+:add-widget tag-layout tag-title 0 0 1 1)
  (setf (q+:style-sheet tag-title) "font-weight: bold;"))

(define-subwidget (description add-tag-button)
    (make-text-qtoolbutton "+")
  (q+:add-widget tag-layout add-tag-button 0 1 1 1))

(define-subwidget (description delete-tag-button)
    (make-text-qtoolbutton "-")
  (q+:add-widget tag-layout delete-tag-button 0 2 1 1))

(define-subwidget (description tag-separator) (q+:make-qframe)
  (q+:add-widget tag-layout tag-separator 1 0 1 4)
  (setf (q+:frame-shape tag-separator) (q+:qframe.hline)
        (q+:frame-shadow tag-separator) (q+:qframe.sunken)))

(define-subwidget (description tag-list) (q+:make-qgridlayout)
  (q+:add-layout tag-layout tag-list 2 0 1 4)
  (setf (q+:column-stretch tag-list 0) 1
        (q+:column-stretch tag-list 1) 5))

(define-subwidget (description add-global-tags-checkbox)
    (q+:make-qcheckbox "Add global tags?")
  (q+:add-widget tag-layout add-global-tags-checkbox 3 0 1 4))

(define-slot (description add-tag-needed) ()
  (declare (connected add-tag-button (clicked)))
  (add-tag description))

(defun add-tag (description)
  (with-all-slots-bound (description description)
    (let ((tag-head (q+:make-qlineedit))
          (tag-body (q+:make-qlineedit)))
      (setf (q+:placeholder-text tag-head) (format nil "#SO")
            (q+:placeholder-text tag-body) (format nil "Awesome stuff!"))
      (q+:add-widget tag-list tag-head ntags 0)
      (q+:add-widget tag-list tag-body ntags 1)
      (incf ntags))))

(define-slot (description delete-tag-needed) ()
  (declare (connected delete-tag-button (clicked)))
  (delete-tag description))

(defun delete-tag (description)
  (with-all-slots-bound (description description)
    (when (< 0 ntags)
      (let ((lines (find-children description 'qlineedit)))
        (finalize (last lines 2))
        (finalize (last lines 1))
        (decf ntags)))))

;;; Spacing

(define-subwidget (description spacer) (q+:make-qwidget)
  (q+:add-widget layout spacer 9001))

;;;

(define-widget afk-description (qwidget)
  ((module :accessor module
           :initarg :module)))

(define-qt-constructor (afk-description)
  (with-slots-bound (module raptor-editor)
    (q+:add-tab tabs afk-description "AFK Description")))

(define-widget afk-looks (qwidget)
  ((module :accessor module
           :initarg :module)))

(define-qt-constructor (afk-looks)
  (with-slots-bound (module raptor-editor)
    (q+:add-tab tabs afk-looks "AFK Looks")))
