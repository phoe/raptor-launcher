;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; color-palette.lisp

(in-package :raptor-launcher/raptor-editor)
(in-readtable :qtools)

;;; TODO fix all DEFINE-WIDGETS so they have %s in slot names
(define-widget color-palette (qwidget palette)
  ((%selector :accessor selector
              :initarg :selector)
   (%color-type :accessor color-type
                :initarg :color-type)
   (%stacked-widget :accessor stacked-widget
                    :initarg :stacked-widget)))

(define-qt-constructor (color-palette name)
  (unless name (error "Must provide NAME."))
  (unless (slot-boundp color-palette '%color-type)
    (error "Must provide COLOR-TYPE."))
  (check-type (color-type color-palette) color)
  (setf (selector color-palette) (make-text-qtoolbutton name))
  (connect! (selector color-palette) (clicked) color-palette (show-widget))
  (add-color-pickers color-palette))

(define-slot (color-palette show-widget) ()
  (let ((widget (stacked-widget color-palette)))
    (setf (q+:current-widget widget) color-palette)))

(define-subwidget (color-palette layout) (q+:make-qgridlayout)
  (setf (q+:layout color-palette) layout
        (q+:contents-margins layout) (values 0 0 0 0)))

(define-subwidget (color-palette gradient) (q+:make-qwidget)
  (q+:add-widget layout gradient 0 0 1 1)
  (setf (q+:style-sheet gradient) "background-color: white;"
        (q+:fixed-width gradient) 64))

(define-subwidget (color-palette scroll) (q+:make-qscrollarea)
  (q+:add-widget layout scroll 0 1 1 1)
  (setf (q+:widget-resizable scroll) t
        (q+:frame-shape scroll) (q+:qframe.no-frame)))

(define-subwidget (color-palette picker) (make-instance 'qui:flow-layout)
  (setf (q+:widget scroll) picker))

(defun add-color-pickers (color-palette)
  (with-all-slots-bound (color-palette color-palette)
    (loop with color-type = (color-type color-palette)
          for color-name in (gethash color-type *color-names*)
          for key = (list color-type color-name)
          for gradient = (gethash key *gradients*)
          for color-picker = (make-instance 'color-picker :color-name color-name
                                                          :gradient gradient)
          do (qui:add-widget color-picker picker))))

(define-widget color-picker (qlabel)
  ((%color-name :accessor color-name
                :initarg :color-name)
   (%gradient :accessor gradient
              :initarg :gradient)
   (%size :accessor size
          :initform 64)))

(define-qt-constructor (color-picker)
  (let* ((size (size color-picker))
         (gradient (rgba-argb (gradient color-picker))))
    (setf (q+:fixed-size color-picker) (values size size)
          (q+:margin color-picker) 2
          (q+:frame-style color-picker) (logxor (q+:qframe.panel)
                                                (q+:qframe.raised))
          (q+:line-width color-picker) 1
          (q+:tool-tip color-picker) (color-name color-picker))
    (with-finalizing
        ((pixmap (with-qimage-from-vector (image gradient 1 256 t)
                   (with-finalizing ((mirror (q+:mirrored image)))
                     (q+:qpixmap-from-image mirror)))))
      (let ((scaled-pixmap (q+:scaled pixmap size size)))
        (setf (q+:pixmap color-picker) scaled-pixmap)))))
