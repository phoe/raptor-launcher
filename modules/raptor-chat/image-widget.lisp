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
  (:default-initargs :foreground-path "" :background-path ""
                     :shadow-path (homepath "shadow.png")
                     :eye-level nil :width nil :background-hue nil))

(define-subwidget (image-widget foreground)
    (q+:make-qpixmap (foreground-path image-widget)))

(define-subwidget (image-widget shadow)
    (q+:make-qpixmap (shadow-path image-widget)))

(define-subwidget (image-widget background)
    (q+:make-qpixmap (background-path image-widget))
  (when (and background-hue (/= 0 background-hue))
    (with-finalizing ((image (q+:to-image background)))
      (dotimes (x (q+:width background))
        (dotimes (y (q+:height background))
          (with-finalizing* ((rgb ;; (fast-call (pixel QImage int int ) image x y)
                              (q+:pixel image x y)
                              )
                             (color (q+:make-qcolor (ldb (byte 8 16) rgb)
                                                    (ldb (byte 8 8) rgb)
                                                    (ldb (byte 8 0) rgb))))
            (let ((hue (q+:hsv-hue color)))
              ;; remember to pull newest qtools!
              (fast-call (set-hsv QColor int int int nil)
                         color
                         (mod (+ hue background-hue) 360)
                         (q+:hsv-saturation color)
                         (q+:value color))
              (setf (q+:pixel image x y) (q+:rgb color))))))
      (let ((old-background background))
        (setf background (q+:qpixmap-from-image image))
        (finalize old-background)))))

#| TODO operate on the whole vector in Lisp to speed this up
https://github.com/wmannis/simple-rgb/blob/master/rgb.lisp
13:51 < Shinmera> qbrush can draw tiled and use a qimage
13:53 < Shinmera> anyway, settexture image and style on qbrush, set qbrush on
qpainter and use fillrech (or what)
|#

(defmethod initialize-instance :after ((object image-widget) &key)
  (with-slots-bound (object image-widget)
    (when width (setf (q+:minimum-width object) width))))

(define-override (image-widget paint-event) (ev)
  (with-finalizing ((painter (q+:make-qpainter image-widget)))
    (q+:draw-tiled-pixmap painter (q+:rect image-widget) background)
    (q+:draw-pixmap painter (q+:rect image-widget) shadow
                    (q+:make-qrect 0 (- (q+:height shadow)
                                        (q+:height (q+:rect image-widget)))
                                   width (q+:height (q+:rect image-widget)))
                    ;; (q+:rect image-widget)
                    )
    (let ((foreground-height (q+:height foreground))
          (height (q+:height image-widget))
          (width (q+:width image-widget))
          (box (q+:rect image-widget))
          result)
      (if (<= foreground-height height)
          (let ((box (q+:rect image-widget)))
            (setf result (q+:make-qrect 0 (- foreground-height height)
                                        width (q+:height box))))
          (let* ((percentage (sqrt (/ height foreground-height)))
                 (y (truncate (- eye-level (* eye-level percentage)))))
            (setf result (q+:make-qrect 0 y width height))))
      (q+:draw-pixmap painter box foreground result))))

(defun image ()
  (with-main-window (image1 (image1))
    (q+:resize image1 150 700)
    (let ((image2 (image2)))
      (q+:show image2)
      (q+:resize image2 150 700))))

(defun image1 ()
  (make-instance
   'image-widget
   :width 150 :eye-level 100 :background-hue 30
   :background-path (homepath "tile.png")
   :shadow-path (homepath "shadow.png")
   :foreground-path (homepath "scaletail.png")))

(defun image2 ()
  (make-instance
   'image-widget
   :width 150 :eye-level 80 :background-hue 30
   :background-path (homepath "tile2.png")
   :shadow-path (homepath "shadow.png")
   :foreground-path (homepath "erchembod.png")))
