;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; loading-screen.lisp

(in-package :raptor-launcher/raptor-picker)
(in-readtable :qtools)

;;; Progress

(define-widget progress (qwidget)
  ((%label-text :accessor label-text
                :initarg :label-text
                :initform "")))

(define-subwidget (progress layout) (q+:make-qvboxlayout)
  (setf (q+:layout progress) layout
        (q+:contents-margins layout) (values 0 4 0 0)))

(define-subwidget (progress label) (q+:make-qlabel)
  (q+:add-widget layout label))

(define-subwidget (progress bar) (q+:make-qprogressbar)
  (q+:add-widget layout bar)
  (setf (q+:range bar) (values 0 0)
        (q+:maximum-height bar) 10))

(defmethod update ((progress progress))
  (with-slots-bound (progress progress)
    (let ((text (format nil "~A: ~D/~D" (label-text progress)
                        (q+:minimum bar) (q+:maximum bar))))
      (setf (q+:text label) text))))

;;; Loading screen

(define-widget loading-screen (qwidget)
  ((%module :accessor module :initarg :module)
   (%progress-logins
    :accessor progress-logins
    :initform (make-instance 'progress :label-text "Accounts logged in"))
   (%progress-accounts
    :accessor progress-accounts
    :initform (make-instance 'progress :label-text "Accounts downloaded"))
   (%progress-furres
    :accessor progress-furres
    :initform (make-instance 'progress :label-text "Furres downloaded"))
   (%progress-portraits
    :accessor progress-portraits
    :initform (make-instance 'progress :label-text "Portraits downloaded"))
   (%progress-specitags
    :accessor progress-specitags
    :initform (make-instance 'progress :label-text "Specitags downloaded"))
   (%progress-costumes
    :accessor progress-costumes
    :initform (make-instance 'progress :label-text "Costumes downloaded"))
   (%progress-images
    :accessor progress-images
    :initform (make-instance 'progress :label-text "Images downloaded"))))

(define-subwidget (loading-screen layout) (q+:make-qvboxlayout)
  (setf (q+:layout loading-screen) layout))

(define-subwidget (loading-screen label) (q+:make-qlabel)
  (q+:add-widget layout label 9001))

(defmethod reset ((loading-screen loading-screen))
  (with-slots-bound (loading-screen loading-screen)
    (let ((symbols '(progress-logins progress-accounts progress-furres
                     progress-portraits progress-specitags
                     progress-costumes progress-images)))
      (loop for symbol in symbols
            for widget = (funcall symbol loading-screen)
            do (update widget)))))

(define-qt-constructor (loading-screen)
  (q+:add-widget (slot-value (module loading-screen) 'layout) loading-screen)
  (let ((symbols '(progress-logins progress-accounts progress-furres
                   progress-portraits progress-specitags
                   progress-costumes progress-images)))
    (loop for symbol in symbols
          for widget = (funcall symbol loading-screen)
          do (q+:add-widget layout widget)))
  (q+:hide loading-screen)
  (reset loading-screen))
