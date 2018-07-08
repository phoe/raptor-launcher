;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; color-palette.lisp

(in-package :raptor-launcher/raptor-editor)
(in-readtable :qtools)

;;; TODO fix all DEFINE-WIDGETS so they have %s in slot names(?)
(define-widget color-palette (qwidget palette)
  ((selector :accessor selector
             :initarg :selector)
   (color-type :accessor color-type
               :initarg :color-type)
   (stacked-widget :accessor stacked-widget
                   :initarg :stacked-widget)))

(define-qt-constructor (color-palette name)
  (unless name (error "Must provide NAME."))
  (unless (slot-boundp color-palette 'color-type)
    (error "Must provide COLOR-TYPE."))
  (check-type color-type color)
  (setf selector (make-text-qtoolbutton name))
  (connect! (selector color-palette) (clicked) color-palette (show-widget))
  (add-color-pickers color-palette))

(define-slot (color-palette show-widget) ()
  (let ((widget stacked-widget))
    (setf (q+:current-widget widget) color-palette)))

(define-subwidget (color-palette layout) (q+:make-qgridlayout)
  (setf (q+:layout color-palette) layout
        (q+:contents-margins layout) (values 0 0 0 0)))

(define-subwidget (color-palette label) (q+:make-qlabel " ")
  (q+:add-widget layout label 0 1 1 1)
  (setf (q+:style-sheet label) "font-weight: bold;"
        (q+:alignment label) (q+:qt.align-center)))

(define-subwidget (color-palette gradient) (q+:make-qlabel)
  (q+:add-widget layout gradient 0 0 2 1)
  (setf (q+:fixed-width gradient) 64
        (q+:scaled-contents gradient) t))

(define-subwidget (color-palette scroll) (q+:make-qscrollarea)
  (q+:add-widget layout scroll 1 1 1 1)
  (setf (q+:widget-resizable scroll) t
        (q+:frame-shape scroll) (q+:qframe.no-frame)))

(define-subwidget (color-palette picker) (make-instance 'qui:flow-layout)
  (setf (q+:widget scroll) picker))

(defun add-color-pickers (color-palette)
  (with-all-slots-bound (color-palette color-palette)
    (loop for color-name in (gethash color-type *color-names*)
          for key = (list color-type color-name)
          for gradient = (gethash key *gradients*)
          for color-picker = (make-instance 'color-picker
                                            :palette color-palette
                                            :widget gradient
                                            :color-type color-type
                                            :color-name color-name
                                            :vector gradient)
          do (qui:add-widget color-picker picker))))
