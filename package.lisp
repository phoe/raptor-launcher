;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; package.lisp

(uiop:define-package #:raptor-launcher
  (:use
   #:cl
   #:raptor-launcher/util
   #:raptor-launcher/config
   #:raptor-launcher/protocol
   #:raptor-launcher/base)
  (:export
   #:main)
  (:reexport
   #:raptor-launcher/util
   #:raptor-launcher/config
   #:raptor-launcher/protocol
   #:raptor-launcher/base))

(in-package #:raptor-launcher)

(defun main ()
  (let ((*main-window* nil)
        (name (format nil "Raptor Launcher ~A" *version*)))
    (qtools:with-main-window (main-window 'raptor-launcher :name name)
      (setf *main-window* main-window)
      (note t :info "Raptor Launcher starting."))))
