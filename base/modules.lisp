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
             (pushnew instance (loaded-modules main-window)))
    (nreversef (loaded-modules main-window))))

(defmethod hide-all-modules ((main-window raptor-launcher))
  (with-slots-bound (main-window raptor-launcher)
    (loop for instance in (loaded-modules main-window)
          for selector = (selector instance)
          for buttons = (buttons instance)
          do (setf (q+:checked selector) nil)
             (q+:hide instance)
             (dolist (button buttons)
               (q+:hide button)))))

(defmethod show-module ((instance module))
  (q+:show instance)
  (dolist (button (buttons instance))
    (q+:show button)))

(defmacro define-raptor-module (name (&rest protocol-classes) &body clauses)
  (let* ((main-window-clause (assoc-value-or-die clauses :main-window))
         (main-window-slots (cdr main-window-clause))
         (selector-clause (assoc-value-or-die clauses :selector))
         (constructor-clause (assoc-value clauses :constructor))
         (pred (lambda (x) (eq (car x) :button)))
         (button-clauses (mapcar #'cdr (remove-if-not pred clauses)))
         (layout-class (first main-window-clause))
         (layout-constructor (symbolicate "MAKE-" layout-class))
         (selector-name (first selector-clause)))
    `(progn
       (define-widget ,name (qwidget ,@protocol-classes)
         ((%main-window :accessor main-window
                        :initform nil)
          (%buttons :accessor buttons)
          (%selector :accessor selector)
          ,@main-window-slots))
       (define-subwidget (,name layout) (q+ ,layout-constructor)
         (setf (q+:layout ,name) layout
               (q+:margin layout) 0))
       (define-subwidget (,name selector) (q+:make-qpushbutton ,selector-name)
         (setf (q+:checkable selector) t))
       ,@(loop for (button-name button-text) in button-clauses
               collect `(define-subwidget (,name ,button-name)
                            (q+:make-qpushbutton ,button-text)))
       (define-slot (,name selected) ()
         (declare (connected selector (pressed)))
         (hide-all-modules (main-window ,name))
         (show-module ,name))
       (define-qt-constructor (,name)
         (setf (selector ,name) selector
               (buttons ,name) (list ,@(mapcar #'car button-clauses)))
         (funcall (lambda () ,@constructor-clause)))
       (pushnew ',name *available-modules*)
       ',name)))
