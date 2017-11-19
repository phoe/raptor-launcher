;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; main-window.lisp

(in-package :raptor-launcher/base)
(in-readtable :qtools)

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

(define-subwidget (main-window module-selector) (q+:make-qwidget))

(define-subwidget (main-window module-selector-layout) (q+:make-qvboxlayout)
  (setf (q+:contents-margins module-selector-layout) (values 0 0 0 0))
  (setf (q+:layout module-selector) module-selector-layout))

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
  (q+:add-widget buttons-layout module-selector)
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
  (load-modules main-window (loaded-modules main-window))
  ;; (unless *available-modules* (error "no modules loaded"))
  )

;;; Logic - definitions

(defvar *available-modules* '()
  "A list of symbols, each naming a Raptor Launcher module class.
\
Each module is required to provide a compatible interface, in form of the
following protocol:
* Each class participating in the protocol must subclass the MODULE protocol
  class. There must exist methods specializing on this class for the following
  generic functions:
* Accessor BUTTONS holding a list of QPushButton instances meant to be shown in
  the module buttons layout.
* Accessor MODULE-SELECTOR holding an instance of MODULE-SELECTOR meant to be
  added to the module selector list.
* Accessor MAIN-WINDOW pointing to the main Raptor Launcher window that the
  instance is loaded into.")

(defclass module (QWidget) ()) ;; TODO turn into protocol

(defgeneric buttons (object)) ;; TODO turn into protocol

(defgeneric module-selector (object)) ;; TODO turn into protocol

(define-signal (main-window show-module) (string)) ;; TODO turn into protocol

(define-slot (main-window show-module) ((module-name string))
  (let ((instance (find module-name (loaded-modules main-window)
                        :test #'string= :key #'type-of)))
    (unless instance (error "Module ~A not found." module-name))
    (hide-all-modules main-window)
    (show-module instance)))

;;; Logic

(defun instantiate-modules (main-window)
  "Instantiates all modules in *AVAILABLE-MODULES* and adds them to the provided
main window."
  (assert (null (loaded-modules main-window)))
  (setf (loaded-modules main-window)
        (mapcar (rcurry #'make-instance :main-window main-window)
                *available-modules*)))

(defun load-modules (main-window instances)
  "Loads all modules from the provided list of instances into the provided main
window."
  (flet ((load-module (main-window instance)
           (with-slots-bound (main-window main-window)
             (let ((selector (module-selector instance))
                   (buttons (buttons instance)))
               (q+:add-widget left-widget-layout instance)
               (q+:hide instance)
               (q+:add-widget module-selector-layout selector)
               (dolist (button buttons)
                 (q+:add-widget module-buttons-layout button)
                 (q+:hide button))
               (setf (main-window instance) main-window)))))
    (loop for instance in instances
          for selector = (module-selector instance)
          do (load-module main-window instance)
             (push instance (loaded-modules main-window)))
    (nreversef (loaded-modules main-window))))

(defun hide-all-modules (main-window)
  (with-slots-bound (main-window main-window)
    (loop for instance in (loaded-modules main-window)
          for buttons = (buttons instance)
          do (q+:hide instance)
             (dolist (button buttons)
               (q+:hide button)))))

(defun show-module (instance)
  (q+:show instance)
  (dolist (button (buttons instance))
    (q+:show button)))

;;; Module selector

;; TODO bugfix, this does not signal anything
(define-widget module-selector (QPushButton)
  ((main-widget :accessor main-widget
                :initarg :main-widget
                :initform nil)
   (module-name :accessor module-name
                :initarg :module-name
                :initform (error "Must provide module name."))))

(define-slot (module-selector pressed) ()
  (declare (connected module-selector (pressed)))
  (print "haha")
  (signal! main-widget (show-module string) module-name))

(defun make-module-selector (button-name module-name &key main-widget)
  (let* ((instance (make-instance 'module-selector
                                  :module-name (string module-name)
                                  :main-widget main-widget)))
    (setf (q+:text instance) button-name)
    instance))

(define-signal (module-selector show-module) (string))

(defun connect-selector (main-window selector)
  (setf (main-widget selector) main-window)
  (with-slots-bound (selector module-selector)
    (connect! selector (show-module string) main-window (show-module string))))

;;; Dummy buttons

(defun make-dummy-button (&rest things)
  (q+:make-qpushbutton (format nil "~{~A~^ ~}" things)))

;;; Test 1

(defpackage :raptor-launcher/test
  (:use #:cl+qt
        #:alexandria
        #:phoe-toolbox
        #:raptor-launcher/util
        #:raptor-launcher/base)
  (:export #:dummy-1 #:dummy-2 #:dummy-3))

(in-package :raptor-launcher/test)
(in-readtable :qtools)

(define-widget dummy-1 (QLabel)
  ((buttons :reader buttons
            :initform (list (make-dummy-button :dummy-1 :a (random 1000))
                            (make-dummy-button :dummy-1 :b (random 1000))
                            (make-dummy-button :dummy-1 :c (random 1000))))
   (main-window :accessor main-window
                :initform nil)
   (module-selector :accessor module-selector)))

(define-subwidget (dummy-1 selector)
    (q+:make-qpushbutton (cat "DUMMY-1 " (prin1-to-string (random 1000)))))

(define-slot (dummy-1 selected) ()
  (declare (connected selector (pressed)))
  (hide-all-modules (main-window dummy-1))
  (show-module dummy-1))

(define-qt-constructor (dummy-1)
  (setf (q+:text dummy-1) (cat "DUMMY-1 " (prin1-to-string (random 1000))))
  (setf (module-selector dummy-1) selector
        (q+:style-sheet dummy-1) "background: blue;"))

;; setStyleSheet("background: red");

(define-widget dummy-2 (QLabel)
  ((buttons :reader buttons
            :initform (list (make-dummy-button :dummy-2 :a (random 1000))
                            (make-dummy-button :dummy-2 :b (random 1000))
                            (make-dummy-button :dummy-2 :c (random 1000))))
   (main-window :accessor main-window
                :initform nil)
   (module-selector :accessor module-selector)))

(define-subwidget (dummy-2 selector)
    (q+:make-qpushbutton (cat "DUMMY-2 " (prin1-to-string (random 1000)))))

(define-slot (dummy-2 selected) ()
  (declare (connected selector (pressed)))
  (hide-all-modules (main-window dummy-2))
  (show-module dummy-2))

(define-qt-constructor (dummy-2)
  (setf (q+:text dummy-2) (cat "DUMMY-2 " (prin1-to-string (random 1000))))
  (setf (module-selector dummy-2) selector
        (q+:style-sheet dummy-2) "background: red;"))

;;; Test 1

(defun test1 ()
  (let ((*available-modules* '(raptor-launcher/test:dummy-1
                               raptor-launcher/test:dummy-2
                               ;; raptor-launcher/test:dummy-3
                               )))
    (with-main-window (main-window 'main-window))))
