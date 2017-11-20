;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; modules.lisp

(in-package :raptor-launcher/base)
(in-readtable :qtools)

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
* Accessor SELECTOR holding an instance of SELECTOR meant to be
  added to the module selector list.
* Accessor MAIN-WINDOW pointing to the main Raptor Launcher window that the
  instance is loaded into.")

(defclass module () ()) ;; TODO turn into protocol class

(defgeneric buttons (object)) ;; TODO turn into protocol

(defgeneric selector (object)) ;; TODO turn into protocol

;;; TODO turn the below into methods and protocolize them

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

(defun hide-all-modules (main-window)
  "Hides all modules inside the provided main window."
  (with-slots-bound (main-window main-window)
    (loop for instance in (loaded-modules main-window)
          for buttons = (buttons instance)
          do (q+:hide instance)
             (dolist (button buttons)
               (q+:hide button)))))

(defun show-module (instance)
  "Shows the provided module instance inside its main window."
  (q+:show instance)
  (dolist (button (buttons instance))
    (q+:show button)))
