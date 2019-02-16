;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; modules.lisp

;;; TODO only keywords in all IN-PACKAGE forms
(in-package :raptor-launcher/base)
(in-readtable :qtools)

(defmethod instantiate-modules ((main-window raptor-launcher)
                                &optional (modules *available-modules*))
  (assert (null (loaded-modules main-window)))
  ;; TODO better ordering here, loggers should go always first
  (let ((modules (reverse modules)))
    (loop for module in modules
          for instance = (make-instance module :main-window main-window)
          do (push instance (loaded-modules main-window)))
    (note t :trace "Modules loaded: ~{~A~^, ~}" modules)))

(defun load-module (main-window instance)
  (with-slots-bound (main-window raptor-launcher)
    (q+:add-widget left-widget-layout instance)
    (q+:hide instance)
    (dolist (button (buttons instance))
      (q+:add-widget module-buttons-layout button)
      (q+:hide button))
    (setf (main-window instance) main-window)))

(defmethod load-modules ((main-window raptor-launcher)
                         &optional (instances (loaded-modules main-window)))
  (loop for instance in instances
        for selector = (selector instance)
        do (load-module main-window instance)
           (pushnew instance (loaded-modules main-window)))
  (insert-selectors main-window)
  (nreversef (loaded-modules main-window)))

(defmethod insert-selectors ((main-window raptor-launcher))
  (with-slots-bound (main-window raptor-launcher)
    (let* ((modules (loaded-modules main-window))
           (modules (sort modules #'< :key #'selector-priority))
           (selectors (mapcar #'selector modules)))
      ;; (loop for module in modules
      ;;       do (format t "~A ~D~%" module (selector-priority module)))
      (loop for i from 0
            for selector in selectors
            do (q+:insert-widget selector-layout i selector)))))

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

(defmethod run-post-init-callbacks ((main-window raptor-launcher))
  (with-slots-bound (main-window raptor-launcher)
    (let* ((modules (loaded-modules main-window))
           (callbacks (apply #'append (mapcar #'post-init-callbacks modules))))
      (mapc #'funcall callbacks))))

;;; TODO extract this to a separate file

(defmacro define-raptor-module (name (&rest protocol-classes) &body clauses)
  (let* ((main-window-clause (assoc-value-or-die clauses :main-window))
         (main-window-slots (cddr main-window-clause))
         (selector-clause (assoc-value-or-die clauses :selector))
         (constructor-clause (assoc-value clauses :constructor))
         (priority (or (car (assoc-value clauses :priority)) 1000))
         (pred (lambda (x) (eq (car x) :button)))
         (button-clauses (mapcar #'cdr (remove-if-not pred clauses)))
         (widget-class (first main-window-clause))
         (layout-class (second main-window-clause))
         (layout-constructor (symbolicate "MAKE-" layout-class))
         (selector-name (first selector-clause)))
    `(progn
       (define-widget ,name (,widget-class ,@protocol-classes)
         ((%main-window :accessor main-window
                        :initform nil)
          (%buttons :accessor buttons)
          (%selector :accessor selector)
          (%config-widget-constructor :accessor config-widget-constructor
                                      :initform nil)
          (%post-init-callbacks :accessor post-init-callbacks
                                :initform '())
          ,@main-window-slots))
       (setf (assoc-value *selector-priorities* ',name) ,priority)
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
