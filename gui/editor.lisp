;;;; editor.lisp

(in-package :furcadia-launcher-gui)
(in-readtable :qtools)

(define-subwidget (launcher editor-box) (q+:make-qlabel "<h1>TODO</h1>")
  (setf (q+:alignment editor-box)
        (q+:qt.align-center)
        (q+:word-wrap editor-box) t))
