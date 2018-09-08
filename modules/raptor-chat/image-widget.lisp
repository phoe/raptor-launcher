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

(define-subwidget (image-widget foreground)
    (q+:make-qimage (foreground-path image-widget)))

(define-subwidget (image-widget shadow)
    (q+:make-qimage (if shadow-path
                        (shadow-path image-widget)
                        "")))

(define-subwidget (image-widget background)
    (q+:make-qimage (background-path image-widget))
  (when (and background-hue (/= 0 background-hue))
    (locally (declare (optimize speed))
      (let ((pointer (q+:bits background))
            (total-size (* (the (unsigned-byte 16) (q+:width background))
                           (the (unsigned-byte 16) (q+:height background)))))
        (dotimes (i total-size)
          (let* ((color (cffi:mem-aref pointer :unsigned-int i))
                 (rgb (make-array 3 :element-type '(unsigned-byte 8)
                                    :initial-contents
                                    (list (ldb (byte 8 16) color)
                                          (ldb (byte 8 8) color)
                                          (ldb (byte 8 0) color))))
                 (rgb2 (the (simple-array (unsigned-byte 8) (3))
                            (simple-rgb:rotate-rgb rgb background-hue)))
                 (result (+ (aref rgb2 0)
                            (ash (aref rgb2 1) 8)
                            (ash (aref rgb2 2) 16)
                            (ash 255 24))))
            (setf (cffi:mem-aref pointer :unsigned-int i) result)))))))

(defmethod initialize-instance :after ((object image-widget) &key)
  (with-slots-bound (object image-widget)
    (when width (setf (q+:minimum-width object) width))))

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

(defun image ()
  (with-main-window (image1 (image1))
    (q+:resize image1 150 800)
    ;; (let ((image2 (image2)))
    ;;   (q+:show image2)
    ;;   (q+:resize image2 150 800))
    ))

(defun image1 ()
  (make-instance
   'image-widget
   :width 150 :eye-level 90 :background-hue 90
   :background-path (homepath "tile.png")
   :shadow-path (homepath "shadow.png")
   :foreground-path (homepath "scaletail.png")))

(defun image2 ()
  (make-instance
   'image-widget
   :width 150 :eye-level 74 :background-hue -60
   :background-path (homepath "tile2.png")
   :shadow-path (homepath "shadow.png")
   :foreground-path (homepath "erchembod.png")))
