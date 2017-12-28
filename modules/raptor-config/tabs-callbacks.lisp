;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; tabs-callbacks.lisp

(in-package :raptor-launcher/raptor-config)
(in-readtable :qtools)

;;; Config tabs - behavior

(define-signal (raptor-config advanced-checkbox-clicked) (bool))

(define-slot (raptor-config toggle-advanced) ((checkedp bool))
  (declare (connected raptor-config (advanced-checkbox-clicked bool)))
  (let ((index (q+:index-of config-tabs advanced-config)))
    (if checkedp
        (q+:add-tab config-tabs advanced-config "Advanced")
        (q+:remove-tab config-tabs index))))

;;; Callbacks

(defun make-callback (raptor-config)
  (with-all-slots-bound (raptor-config raptor-config)
    (lambda ()
      (let* ((modules (loaded-modules (main-window raptor-config)))
             (constructors (mapcar #'config-widget-constructor modules))
             (widgets (mapcar #'funcall (remove nil constructors))))
        (mapc (lambda (x) (q+:add-widget simple-config-layout x))
              widgets)))))

(defun make-spacer (raptor-config)
  (with-all-slots-bound (raptor-config raptor-config)
    (lambda ()
      (let ((layout (q+:layout simple-config-layout)))
        (q+:add-stretch layout 9001)))))
