;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; modules.lisp

(in-package :raptor-launcher/base)
(in-readtable :qtools)

(defmethod instantiate-modules ((main-window raptor-launcher)
                                &optional (modules *available-modules*))
  (assert (null (loaded-modules main-window)))
  (setf (loaded-modules main-window)
        (mapcar (rcurry #'make-instance :main-window main-window) modules)))

(defmethod load-modules ((main-window raptor-launcher)
                         &optional (instances (loaded-modules main-window)))
  (flet ((load-module (main-window instance)
           (with-slots-bound (main-window raptor-launcher)
             (let ((selector (selector instance))
                   (buttons (buttons instance)))
               (q+:add-widget left-widget-layout instance)
               (q+:hide instance)
               (q+:add-widget selector-layout selector)
               (dolist (button buttons)
                 (q+:add-widget module-buttons-layout button)
                 (q+:hide button))
               (setf (main-window instance) main-window)))))
    (loop for instance in instances
          for selector = (selector instance)
          do (load-module main-window instance)
             (push instance (loaded-modules main-window)))
    (nreversef (loaded-modules main-window))))

(defmethod hide-all-modules ((main-window raptor-launcher))
  (with-slots-bound (main-window raptor-launcher)
    (loop for instance in (loaded-modules main-window)
          for buttons = (buttons instance)
          do (q+:hide instance)
             (dolist (button buttons)
               (q+:hide button)))))

(defmethod show-module ((instance module))
  (q+:show instance)
  (dolist (button (buttons instance))
    (q+:show button)))
