;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; utils.lisp

(in-package :raptor-launcher/base)
(in-readtable :qtools)

(defun (setf table-text) (new-value widget row column &optional editablep)
  "Sets the text of the given QTableView widget at ROW and COLUMN to TEXT.
If EDITABLEP, sets the widget to be editable."
  (when (null-qobject-p (q+:item widget row column))
    (setf (q+:item widget row column) (q+:make-qtablewidgetitem)))
  (let ((item (q+:item widget row column)))
    (setf (q+:text item) new-value
          (q+:flags item) (+ (q+:qt.item-is-selectable)
                             (q+:qt.item-is-enabled)
                             (if editablep (q+:qt.item-is-editable) 0)))))

(defun make-text-qtoolbutton (text)
  (let ((button (q+:make-qtoolbutton)))
    (setf (q+:text button) text
          (q+:tool-button-style button)
          (q+:qt.tool-button-text-only))
    button))

(defmacro with-qimage-from-vector
    ((image-var vector width height &optional re-endian) &body body)
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
