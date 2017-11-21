;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; main-window.lisp

(in-package :raptor-launcher/base)
(in-readtable :qtools)

;;; TODO define protocol and symbols exported by /BASE
;;; TODO create RAPTOR-LAUNCHER/PROTOCOL system

;;; Class declaration

(define-widget main-window (QMainWindow)
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

(define-subwidget (main-window central-widget) (q+:make-qwidget)
  (setf (q+:central-widget main-window) central-widget))

;;; Module selector

(define-subwidget (main-window selector) (q+:make-qwidget))

(define-subwidget (main-window selector-layout) (q+:make-qvboxlayout)
  (setf (q+:contents-margins selector-layout) (values 0 0 0 0))
  (setf (q+:layout selector) selector-layout))

;;; Button separator

(define-subwidget (main-window button-separator) (q+:make-qwidget)
  (setf (q+:size-policy button-separator) (values (q+:qsizepolicy.fixed)
                                                  (q+:qsizepolicy.expanding))))

;;; Module buttons

(define-subwidget (main-window module-buttons) (q+:make-qwidget))

(define-subwidget (main-window module-buttons-layout) (q+:make-qvboxlayout)
  (setf (q+:contents-margins module-buttons-layout) (values 0 0 0 0))
  (setf (q+:layout module-buttons) module-buttons-layout))

;;; Quit button

(define-subwidget (main-window main-quit-button) (q+:make-qpushbutton "Quit"))

(define-slot (main-window quit) ()
  (declare (connected main-quit-button (pressed)))
  (q+:close main-window))

;;; Buttons layour

(define-subwidget (main-window buttons-layout) (q+:make-qvboxlayout)
  (q+:add-widget buttons-layout selector)
  (q+:add-widget buttons-layout button-separator)
  (q+:add-widget buttons-layout module-buttons)
  (q+:add-widget buttons-layout main-quit-button)
  (setf (q+:contents-margins buttons-layout) (values 0 0 0 0)))

;;; Left main widget (module content)

(define-subwidget (main-window left-widget) (q+:make-qwidget)
  (setf (q+:size-policy left-widget) (values (q+:qsizepolicy.expanding)
                                             (q+:qsizepolicy.expanding))))

(define-subwidget (main-window left-widget-layout) (q+:make-qvboxlayout)
  (setf (q+:layout left-widget) left-widget-layout
        (q+:contents-margins left-widget-layout) (values 0 0 0 0)))

;;; Right main widget (buttons)

(define-subwidget (main-window right-widget) (q+:make-qwidget)
  (setf (q+:layout right-widget) buttons-layout))

;;; Central layout

(define-subwidget (main-window central-layout) (q+:make-qhboxlayout)
  (q+:add-widget central-layout left-widget)
  (q+:add-widget central-layout right-widget))

;;; Main window constructor

(define-qt-constructor (main-window)
  (let ((title (format nil "Raptor Launcher ~A" *version*)))
    (setf (q+:window-title main-window) title
          (q+:minimum-size main-window) (values 600 600)
          (q+:layout central-widget) central-layout))
  (instantiate-modules main-window)
  (load-modules main-window (loaded-modules main-window)))
