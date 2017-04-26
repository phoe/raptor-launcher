;;;; gui.lisp

(in-package :furcadia-launcher-gui)
(in-readtable :qtools)

(defun logger-build-hook ()
  (kill *logger*)
  (loop until (not (alivep *logger*)) do (sleep 1)))

(defun logger-boot-hook ()
  (setf *logger* (make-instance 'logger)))

(pushnew 'logger-build-hook qtools:*build-hooks*)

(pushnew 'logger-boot-hook qtools:*boot-hooks*)

(defvar *current-selection* nil)

(define-widget main-window (QMainWindow) ())

(define-subwidget (main-window central-widget) (q+:make-qwidget)
  (setf (q+:central-widget main-window) central-widget))

(define-subwidget (main-window list-widget) (make-furre-list-widget))

(define-subwidget (main-window button-play) (q+:make-qpushbutton "Play!"))

(define-subwidget (main-window button-quit) (q+:make-qpushbutton "Quit"))

(define-subwidget (main-window grid-layout) (q+:make-qgridlayout)
  (setf (q+:layout central-widget) grid-layout)
  (q+:add-widget grid-layout list-widget 1 0 2 2)
  (q+:add-widget grid-layout button-play 0 0)
  (q+:add-widget grid-layout button-quit 0 1))

(define-signal (main-window furre-changed) ("int"))

(define-signal (main-window launch-done) ())

(define-slot (main-window furre-changed) ()
  (declare (connected list-widget (current-row-changed "int")))
  (let* ((list-item (q+:current-item list-widget))
         (text (q+:text list-item)))
    (setf *current-selection* text)))

(define-slot (main-window quit-launcher) ()
  (declare (connected button-quit (clicked)))
  (q+:close main-window))

(define-slot (main-window start-furcadia) ()
  (declare (connected button-play (clicked)))
  (setf (q+:enabled button-play) nil
        (q+:enabled button-quit) nil)
  (bt:make-thread (lambda ()
                    (furcadia-launcher::furcadia *current-selection*)
                    (signal! main-window (launch-done)))))

(define-slot (main-window enable-buttons) ()
  (declare (connected main-window (launch-done)))
  (setf (q+:enabled button-play) t
        (q+:enabled button-quit) t))

;; (with-main-window (window 'main-window))

(defun get-names ()
  (let* ((config furcadia-launcher::*config*)
         (characters (getf config :characters))
         (snames (mapcar #'car characters))
         (sorted-names (sort (copy-list snames) #'string<)))
    sorted-names))

(defun make-furre-list-widget ()
  (let ((list-widget (q+:make-qlistwidget)))
    (loop for name in (get-names)
          for i from 0
          do (q+:insert-item list-widget i name))
    list-widget))

(defun main ()
  (with-main-window (window 'main-window)
    (setf (q+:window-title window) "Launcher"
          (q+:fixed-size window) (values 200 400))))

(defun build-main ()
  (furcadia-launcher::initialize)
  (main))

;; (:accounts (("foo@bar.baz" "thisispassword")
;;             ("foo@bar.baz" "thisispassword")
;;             ("foo@bar.baz" "thisispassword")
;;             ("foo@bar.baz" "thisispassword"))
;;  :FURCADIA-PATH "C:\\Program Files (x86)\\Furcadia\\")
