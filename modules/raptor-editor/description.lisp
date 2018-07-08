;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; description.lisp

(in-package :raptor-launcher/raptor-editor)
(in-readtable :qtools)

(define-widget description (qwidget)
  ((module :accessor module
           :initarg :module)
   (name :accessor name
         :initarg :name)
   (ntags :accessor ntags
          :initform 0)))

(define-qt-constructor (description)
  (with-slots (tabs) module
    (note t :trace "Raptor Editor: ~A instantiated." name)
    (q+:add-tab tabs description name)))

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

(define-subwidget (description spacer) (q+:make-qwidget)
  (q+:add-widget layout spacer 9001))
