;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; main-window.lisp

(in-package :raptor-launcher/base)
(in-readtable :qtools)

;;; Class declaration

(define-widget raptor-launcher (QMainWindow)
  ((loaded-modules :accessor loaded-modules
                   :initform '()))
  (:documentation "The main window widget for Raptor Launcher.
\
The main window contains a single CENTRAL-WIDGET that is split into two. On the
left we have the LEFT-WIDGET with LEFT-WIDGET-LAYOUT that holds the contents of
the currently loaded module. On the right, we have the RIGHT-WIDGET with
RIGHT-WIDGET-LAYOUT that holds the launcher buttons in BUTTONS-LAYOUT.
\
The BUTTONS-LAYOUT has three types of buttons. The bottommost button is the Quit
button that exits the launcher. The lower buttons are module-specific buttons,
defined by the module itself. The upper buttons are the module selector buttons,
each of which shows the contents of the selected module in the
LEFT-WIDGET-LAYOUT and shows the module-specific buttons."))

(define-subwidget (raptor-launcher central-widget) (q+:make-qwidget)
  (setf (q+:central-widget raptor-launcher) central-widget))

;;; Module selector

(define-subwidget (raptor-launcher selector) (q+:make-qwidget))

(define-subwidget (raptor-launcher selector-layout) (q+:make-qvboxlayout)
  (setf (q+:contents-margins selector-layout) (values 0 0 0 0))
  (setf (q+:layout selector) selector-layout))

;;; Button separator

(define-subwidget (raptor-launcher button-separator) (q+:make-qwidget)
  (setf (q+:size-policy button-separator) (values (q+:qsizepolicy.fixed)
                                                  (q+:qsizepolicy.expanding))))

;;; Module buttons

(define-subwidget (raptor-launcher module-buttons) (q+:make-qwidget))

(define-subwidget (raptor-launcher module-buttons-layout) (q+:make-qvboxlayout)
  (setf (q+:contents-margins module-buttons-layout) (values 0 0 0 0))
  (setf (q+:layout module-buttons) module-buttons-layout))

;;; Quit button

(define-subwidget (raptor-launcher main-quit-button)
    (q+:make-qpushbutton "Quit"))

(define-slot (raptor-launcher quit) ()
  (declare (connected main-quit-button (pressed)))
  (q+:close raptor-launcher))

;;; Buttons layour

(define-subwidget (raptor-launcher buttons-layout) (q+:make-qvboxlayout)
  (q+:add-widget buttons-layout selector)
  (q+:add-widget buttons-layout button-separator)
  (q+:add-widget buttons-layout module-buttons)
  (q+:add-widget buttons-layout main-quit-button)
  (setf (q+:contents-margins buttons-layout) (values 0 0 0 0)))

;;; Left main widget (module content)

(define-subwidget (raptor-launcher left-widget) (q+:make-qwidget)
  (setf (q+:size-policy left-widget) (values (q+:qsizepolicy.expanding)
                                             (q+:qsizepolicy.expanding))))

(define-subwidget (raptor-launcher left-widget-layout) (q+:make-qvboxlayout)
  (setf (q+:layout left-widget) left-widget-layout
        (q+:contents-margins left-widget-layout) (values 0 0 0 0)))

;;; Right main widget (buttons)

(define-subwidget (raptor-launcher right-widget) (q+:make-qwidget)
  (setf (q+:layout right-widget) buttons-layout))

;;; Central layout

(define-subwidget (raptor-launcher central-layout) (q+:make-qhboxlayout)
  (q+:add-widget central-layout left-widget)
  (q+:add-widget central-layout right-widget))

;;; Main window constructor

(define-qt-constructor (raptor-launcher)
  (let ((title (format nil "Raptor Launcher ~A" *version*)))
    (setf (q+:window-title raptor-launcher) title
          (q+:minimum-size raptor-launcher) (values 600 600)
          (q+:layout central-widget) central-layout))
  (instantiate-modules raptor-launcher)
  (load-modules raptor-launcher))
