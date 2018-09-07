;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; image-widget.lisp

(in-package :raptor-launcher/raptor-chat)
(in-readtable :qtools)

(define-widget image-widget (qlabel)
  ((foreground-path :accessor foreground-path :initarg :foreground-path)
   (shadow-path :accessor shadow-path :initarg :shadow-path)
   (background-path :accessor background-path :initarg :background-path)
   (eye-level :accessor eye-level :initarg :eye-level)
   (width :accessor width :initarg :width)
   (background-hue :accessor background-hue :initarg :background-hue))
  (:default-initargs :foreground-path "" :shadow-path "" :background-path ""
                     :eye-level nil :width nil :background-hue nil))

(define-subwidget (image-widget foreground)
    (q+:make-qpixmap (foreground-path image-widget)))

(define-subwidget (image-widget shadow)
    (q+:make-qpixmap (shadow-path image-widget)))

(define-subwidget (image-widget background)
    (q+:make-qpixmap (background-path image-widget))
  (when background-hue
    (with-finalizing ((image (q+:to-image background)))
      (dotimes (x (q+:width background))
        (dotimes (y (q+:height background))
          (with-finalizing* ((rgb (q+:pixel image x y))
                             (color (q+:make-qcolor (ldb (byte 8 16) rgb)
                                                    (ldb (byte 8 8) rgb)
                                                    (ldb (byte 8 0) rgb))))
            (let ((hue (q+:hue color)))
              (setf (q+:hsv color) (values (+ hue background-hue)
                                           (q+:saturation color)
                                           (q+:value color)
                                           (q+:alpha color)))
              (setf (q+:pixel image x y) (q+:rgb color))))))
      (let ((old-background background))
        (setf background (q+:qpixmap-from-image image))
        (finalize old-background)))))

(defmethod initialize-instance :after ((object image-widget) &key)
  (with-slots-bound (object image-widget)
    (when width (setf (q+:minimum-width object) width))))

(define-override (image-widget paint-event) (ev)
  (with-finalizing ((painter (q+:make-qpainter image-widget)))
    (q+:draw-tiled-pixmap painter (q+:rect image-widget) background)
    (q+:draw-pixmap painter (q+:rect image-widget) shadow
                    (q+:make-qrect 0 0 0 0))
    (let ((foreground-height (q+:height foreground))
          (height (q+:height image-widget))
          (width (q+:width image-widget))
          (box (q+:rect image-widget))
          result)
      (if (<= foreground-height height)
          (let ((box (q+:rect image-widget)))
            (setf result (q+:make-qrect 0 (- foreground-height height)
                                        width (q+:height box))))
          (let ((y (truncate (- eye-level
                                (* eye-level (/ height foreground-height))))))
            (setf result (q+:make-qrect 0 y width height))))
      (q+:draw-pixmap painter box foreground result))))

(defun image ()
  (with-main-window
      (image (make-instance
              'image-widget
              :width 150 :eye-level 77 :background-hue 180
              :background-path "/home/phoe/Projects/Raptor Chat/tile.png"
              :shadow-path "/home/phoe/Projects/Raptor Chat/shadow.png"
              :foreground-path "/home/phoe/Projects/Raptor Chat/erchembod.png"
              ))))
