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

(defmacro define-loading-screen (name (qt-class &rest direct-superclasses)
                                 direct-slots progress-bars &rest options)
  `(define-widget ,name (,qt-class ,@direct-superclasses)
     (,@direct-slots
      ,@(loop for (accessor label-text) in progress-bars
              collect `(,(symbolicate "%" accessor)
                        :accessor ,accessor
                        :initform (make-instance 'progress
                                                 :label-text ,label-text))))
     ,@options))

(trivial-indent:define-indentation define-loading-screen
    (4 4 &rest 2))

(define-loading-screen loading-screen (qwidget)
  ((%module :accessor module :initarg :module))
  ((progress-logins "Accounts logged in")
   (progress-accounts "Accounts downloaded")
   (progress-furres "Furres downloaded")
   (progress-portraits "Portraits downloaded")
   (progress-specitags "Specitags downloaded")
   (progress-costumes "Costumes downloaded")
   (progress-images "Images downloaded")))

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
