;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; package.lisp

(uiop:define-package #:raptor-launcher
  (:use
   #:cl
   #:alexandria
   #:named-readtables
   #:raptor-launcher/util
   #:raptor-launcher/config
   #:raptor-launcher/protocol
   #:raptor-launcher/base
   )
  (:export
   #:main))

(in-package #:raptor-launcher)
(in-readtable :qtools)

(defun main ()
  (let ((*main-window* nil)
        (name (format nil "Raptor Launcher ~A" *version*)))
    (qtools:with-main-window (main-window 'raptor-launcher :name name)
      (note t :info "Raptor Launcher started.")
      (qtools:with-slots-bound (main-window raptor-launcher)
        (setf (assoc-value bt:*default-special-bindings* '*main-window*)
              main-window)
        ;; TODO *default-open-module* variable
        (let* ((layout-item (q+:item-at
                             raptor-launcher/base::selector-layout
                             0)))
          (q+:click (q+:widget layout-item)))))
    (removef bt:*default-special-bindings* '*main-window* :key #'car)))
