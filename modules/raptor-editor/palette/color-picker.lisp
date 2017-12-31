;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; color-picker.lisp

(in-package :raptor-launcher/raptor-editor)
(in-readtable :qtools)

(define-widget color-picker (qpushbutton)
  ((%palette :accessor palette
             :initarg :palette)
   (%color-name :accessor color-name
                :initarg :color-name)
   (%color-type :accessor color-type
                :initarg :color-type)
   (%vector :accessor gradient-vector
            :initarg :vector)
   (%widget :accessor gradient-widget
            :initarg :widget)
   (%size :accessor size ;;; TODO make this configurable
          :initform 64)))

(define-qt-constructor (color-picker)
  (let* ((size (size color-picker))
         (pixsize (- size 4))
         (vector (rgba-argb (gradient-vector color-picker))))
    (setf (q+:fixed-size color-picker) (values size size)
          (q+:flat color-picker) t
          (q+:tool-tip color-picker) (color-name color-picker))
    (with-finalizing ((pixmap (with-qimage-from-vector (image vector 1 256 t)
                                (with-finalizing ((mirror (q+:mirrored image)))
                                  (q+:qpixmap-from-image mirror)))))
      (let ((scaled-pixmap (q+:scaled pixmap pixsize pixsize)))
        (setf (q+:icon color-picker) (q+:make-qicon scaled-pixmap)
              (q+:icon-size color-picker) (q+:make-qsize pixsize pixsize))))))

(define-slot (color-picker color-picker-clicked) ()
  (declare (connected color-picker (clicked)))
  (q+:clear-focus color-picker)
  (signal! (palette color-picker)
           (color-picked color-picker) color-picker))

(define-signal (color-palette color-picked) (color-picker))

(define-slot (color-palette color-picker-clicked) ((color-picker color-picker))
  (declare (connected color-palette (color-picked color-picker)))
  (let* ((vector (rgba-argb (gradient-vector color-picker))))
    (with-finalizing ((pixmap (with-qimage-from-vector (image vector 1 256 t)
                                (with-finalizing ((mirror (q+:mirrored image)))
                                  (q+:qpixmap-from-image mirror)))))
      (setf (q+:pixmap gradient) pixmap))))
