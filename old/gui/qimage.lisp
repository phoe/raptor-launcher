;;;; qimage.lisp

(in-package :furcadia-launcher-gui)
(in-readtable :qtools)

(define-widget image-window (QMainWindow)
  ())

(define-subwidget (image-window image) (q+:make-qimage))

(defun show (vector height width &optional (re-endian t))
  (check-type vector (vector (unsigned-byte 8)))
  (check-type height unsigned-byte)
  (check-type width unsigned-byte)
  (with-main-window (window 'image-window)
    (with-qimage-from-vector (image vector width height re-endian)
      (let ((label (q+:make-qlabel))
            (pixmap (q+:qpixmap-from-image image)))
        (q+:save pixmap "/tmp/foo.png")
        (setf (q+:pixmap label) pixmap
              (q+:central-widget window) label)))))

(defmacro with-qimage-from-vector ((image-var vector width height
                                    &optional re-endian) &body body)
  (with-gensyms (length v re-endian-p)
    `(let* ((,length (array-dimension ,vector 0))
            (,re-endian-p ,re-endian)
            (,v (make-static-vector ,length :initial-contents ,vector)))
       (when ,re-endian-p (re-endian-32 ,v))
       (unwind-protect
            (with-finalizing
                ((,image-var (q+:make-qimage
                              (static-vector-pointer ,v)
                              ,width ,height
                              (q+:qimage.format_argb32))))
              ,@body)
         (free-static-vector ,v)))))

(defun re-endian-32 (vector)
  (loop for i below (length vector) by 4
        do (rotatef (aref vector i) (aref vector (+ i 3)))
           (rotatef (aref vector (+ i 1)) (aref vector (+ i 2))))
  vector)
