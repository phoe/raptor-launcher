;;;; qimage.lisp

(in-package :furcadia-launcher-gui)
(in-readtable :qtools)

(define-widget image-window (QMainWindow)
  ())

(define-subwidget (image-window image) (q+:make-qimage))

(defun show (vector height width)
  (check-type vector (vector (unsigned-byte 8)))
  (check-type height unsigned-byte)
  (check-type width unsigned-byte)
  (with-main-window (window 'image-window)
    (with-qimage-from-vector (image vector height width)
      (let ((label (q+:make-qlabel)))
        (setf (q+:pixmap label) (q+:qpixmap-from-image image)
              (q+:central-widget window) label)
        (q+:show label)))))

(defmacro with-qimage-from-vector ((image-var vector width height) &body body)
  (with-gensyms (length v)
    `(let* ((,length (array-dimension ,vector 0))
            (,v (make-static-vector ,length :initial-contents vector)))
       (unwind-protect
            (with-finalizing
                ((,image-var (q+:make-qimage
                              (static-vector-pointer ,v)
                              ,width ,height
                              (q+:qimage.format_argb32))))
              ,@body)
         (free-static-vector ,v)))))
