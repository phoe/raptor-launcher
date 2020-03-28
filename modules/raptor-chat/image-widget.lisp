;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; image-widget.lisp

(in-package :raptor-launcher/raptor-chat)
(in-readtable :qtools)

(define-widget image-widget (qwidget)
  ((foreground-path :accessor foreground-path :initarg :foreground-path)
   (shadow-path :accessor shadow-path :initarg :shadow-path)
   (background-path :accessor background-path :initarg :background-path)
   (eye-level :accessor eye-level :initarg :eye-level)
   (width :accessor width :initarg :width)
   (background-hue :accessor background-hue :initarg :background-hue))
  (:default-initargs :foreground-path "" :background-path ""
                     :shadow-path (homepath "shadow.png")
                     :eye-level nil :width nil :background-hue nil))

(define-subwidget (image-widget foreground) (q+:make-qimage foreground-path))

(define-subwidget (image-widget shadow) (q+:make-qimage shadow-path))

(define-subwidget (image-widget background) (q+:make-qimage background-path)
  (when (and background-hue (/= 0.0 background-hue))
    (qui:hue-shift background background-hue)))

(define-qt-constructor (image-widget)
  (when width (setf (q+:minimum-width image-widget) width)))

(define-override (image-widget paint-event) (ev)
  (with-finalizing ((painter (q+:make-qpainter image-widget)))
    (setf (q+:render-hint painter) (q+:qpainter.antialiasing))
    (q+:draw-tiled-pixmap painter (q+:rect image-widget)
                          (q+:qpixmap-from-image background))
    (let ((box (q+:rect image-widget))
          (height (q+:height image-widget))
          (width (q+:width image-widget))
          (foreground-height (q+:height foreground))
          result)
      (q+:draw-image
       painter box shadow
       (q+:make-qrect 0 (- (q+:height shadow) height) width height))
      (if (<= foreground-height height)
          (let ((box (q+:rect image-widget)))
            (setf result (q+:make-qrect 0 (- foreground-height height)
                                        width (q+:height box))))
          (let* ((percentage (/ height foreground-height))
                 (y (truncate (- eye-level (* eye-level percentage)))))
            (setf result (q+:make-qrect 0 y width height))))
      (q+:draw-image painter box foreground result))))

(defun image1 ()
  (make-instance
   'image-widget
   :width 150 :eye-level 74 :background-hue 300.0
   :background-path (homepath "tile2.png")
   :shadow-path (homepath "shadow.png")
   :foreground-path (homepath "erchembod.png")))

(defun image2 ()
  (make-instance
   'image-widget
   :width 150 :eye-level 74 :background-hue 80.0
   :background-path (homepath "tile.png")
   :shadow-path (homepath "shadow.png")
   :foreground-path (homepath "scaletail.png")))

(defun image ()
  (with-main-window (widget (q+:make-qwidget))
    (let ((layout (q+:make-qhboxlayout)))
      (setf (q+:layout widget) layout)
      (q+:add-widget layout (%image 0.0))
      (q+:add-widget layout (%image 60.0))
      (q+:add-widget layout (%image 120.0))
      (q+:add-widget layout (%image 180.0))
      (q+:add-widget layout (%image 240.0))
      (q+:add-widget layout (%image 300.0))
      (q+:resize widget 1 1000))))

(defun %image (&optional (hue 0.0))
  (make-instance
   'image-widget
   :width 150 :eye-level 74 :background-hue hue
   :background-path (homepath "tile2.png")
   :shadow-path (homepath "shadow.png")
   :foreground-path (homepath "erchembod.png")))
