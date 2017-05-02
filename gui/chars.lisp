;;;; chars.lisp

(in-package :furcadia-launcher-gui)
(in-readtable :qtools)

(define-subwidget (launcher character-list) (q+:make-qtablewidget)
  (setf (q+:minimum-height character-list) 100))

(define-subwidget (launcher description-preview) (q+:make-qtextedit)
  (setf (q+:minimum-height description-preview) 100))

(define-subwidget (launcher chars-box) (q+:make-qsplitter)
  (q+:add-widget chars-box character-list)
  (q+:add-widget chars-box description-preview)
  (setf (q+:orientation chars-box) (q+:qt.vertical)
        (q+:children-collapsible chars-box) nil
        (q+:stretch-factor chars-box) (values 0 2)
        (q+:stretch-factor chars-box) (values 1 1)))
