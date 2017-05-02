;;;; debug.lisp

(in-package :furcadia-launcher-gui)
(in-readtable :qtools)


(defparameter *debug-intro*
  "; This is the debug read-eval-print-loop of the Common Lisp image running ~
underneath the launcher. Warning: typing stuff here CAN MAKE YOUR LAUNCHER ~
EXPLODE. If you are sure that you want to use it - delete this comment and ~
happy hacking!")

(define-subwidget (launcher debug-logs) (q+:make-qtextbrowser))

(define-subwidget (launcher debug-box) (q+:make-qsplitter)
  (q+:add-widget debug-box debug-logs)
  (let* ((evaluator (make-instance 'qtools-evaluator::evaluator))
         (layout (find-child evaluator (find-qclass "QVBoxLayout")))
         (repl (find-child evaluator (find-qclass "QTextEdit")))
         (stream (qtools-evaluator::repl-output-stream evaluator))
         (font (q+:make-qfont "Monospace" 7 50 nil)))
    (q+:add-widget debug-box evaluator)
    (setf (q+:contents-margins layout) (values 0 0 0 0)
          (q+:style-hint font) (q+:qfont.type-writer)
          (q+:font repl) font
          (q+:orientation debug-box) (q+:qt.vertical)
          (q+:children-collapsible debug-box) nil
          (q+:stretch-factor debug-box) (values 0 5)
          (q+:stretch-factor debug-box) (values 1 1))
    (format stream *debug-intro*)))
