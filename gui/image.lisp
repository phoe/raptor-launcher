;;;; image.lisp

(in-package :furcadia-launcher-gui)
(in-readtable :qtools)

(define-subwidget (launcher image) (q+:make-qpushbutton "No Character Image
\(click here to add)
\(150x400)
\(not working yet)")
  (setf (q+:minimum-width image) 150
        (q+:maximum-width image) 150
        (q+:flat image) t
        (q+:size-policy image) (values (q+:qsizepolicy.expanding)
                                       (q+:qsizepolicy.expanding))))
