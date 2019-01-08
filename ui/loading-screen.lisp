;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; loading-screen.lisp

(in-package :raptor-launcher/ui)
(in-readtable :qtools)

;;; Progress

(define-widget progress (qwidget)
  ((label-text :accessor label-text
               :initarg :label-text
               :initform "")))

(defmethod (setf label-text) :after (new-value (progress progress))
  (update progress))

(defun make-progress (label-text)
  (make-instance 'progress :label-text label-text))

(define-subwidget (progress layout) (q+:make-qvboxlayout)
  (setf (q+:layout progress) layout
        (q+:contents-margins layout) (values 0 4 0 0)))

(define-subwidget (progress label) (q+:make-qlabel)
  (q+:add-widget layout label))

(define-subwidget (progress bar) (q+:make-qprogressbar)
  (q+:add-widget layout bar)
  (setf (q+:text-visible bar) nil
        (q+:range bar) (values 0 0)
        (q+:maximum-height bar) 10))

(defun update (progress)
  (with-slots-bound (progress progress)
    (let ((text (format nil "~A: ~D/~D" (label-text progress)
                        (q+:value bar) (q+:maximum bar))))
      (setf (q+:text label) text))))

(defmethod reset ((progress progress))
  (with-slots-bound (progress progress)
    (q+:reset bar)
    (setf (q+:minimum bar) 0
          (q+:value bar) 0
          (q+:maximum bar) 0)
    (update progress)
    nil))

(defun progress-maximum (progress)
  (with-slots-bound (progress progress)
    (q+:maximum bar)))

(defun (setf progress-maximum) (new-value progress)
  (with-slots-bound (progress progress)
    (prog1 (setf (q+:maximum bar) new-value)
      (update progress))))

(defun progress-minimum (progress)
  (with-slots-bound (progress progress)
    (q+:minimum bar)))

(defun (setf progress-minimum) (new-value progress)
  (with-slots-bound (progress progress)
    (prog1 (setf (q+:minimum bar) new-value)
      (update progress))))

(defun progress-current (progress)
  (with-slots-bound (progress progress)
    (q+:value bar)))

(defun (setf progress-current) (new-value progress)
  (with-slots-bound (progress progress)
    (prog1 (setf (q+:value bar) new-value)
      (update progress))))

;;; Loading screen

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *progress-types*
    '(progress-logins progress-accounts progress-furres progress-portraits
      progress-specitags progress-costumes progress-images)))

(deftype progress-type ()
  '#.`(member ,@*progress-types*))

(defmacro define-loading-screen (name (qt-class &rest direct-superclasses)
                                 direct-slots progress-bars &rest options)
  (let ((bars (loop for (accessor label-text) in progress-bars
                    collect `(,(symbolicate "%" accessor)
                              :accessor ,accessor
                              :initform (make-progress ,label-text)))))
    `(define-widget ,name (,qt-class ,@direct-superclasses)
       (,@direct-slots ,@bars) ,@options)))

(define-loading-screen loading-screen (qwidget) ()
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

(defmethod initialize-instance :after
    ((loading-screen loading-screen) &key &allow-other-keys)
  (with-slots-bound (loading-screen loading-screen)
    (loop for symbol in *progress-types*
          for widget = (funcall symbol loading-screen)
          do (q+:add-widget layout widget))
    (reset loading-screen)))

(defmethod reset ((loading-screen loading-screen))
  (with-slots-bound (loading-screen loading-screen)
    (loop for symbol in *progress-types*
          for widget = (funcall symbol loading-screen)
          do (reset widget))))

(defmethod maximum ((screen loading-screen) progress-type)
  (check-type progress-type progress-type)
  (let ((progress (funcall progress-type screen)))
    (progress-maximum progress)))

(defmethod (setf maximum) (new-value (screen loading-screen) progress-type)
  (check-type progress-type progress-type)
  (let ((progress (funcall progress-type screen)))
    (setf (progress-maximum progress) new-value)))

(defmethod minimum ((screen loading-screen) progress-type)
  (check-type progress-type progress-type)
  (let ((progress (funcall progress-type screen)))
    (progress-minimum progress)))

(defmethod (setf minimum) (new-value (screen loading-screen) progress-type)
  (check-type progress-type progress-type)
  (let ((progress (funcall progress-type screen)))
    (setf (progress-minimum progress) new-value)))

(defmethod current ((screen loading-screen) progress-type)
  (check-type progress-type progress-type)
  (let ((progress (funcall progress-type screen)))
    (progress-current progress)))

(defmethod (setf current) (new-value (screen loading-screen) progress-type)
  (check-type progress-type progress-type)
  (let ((progress (funcall progress-type screen)))
    (setf (progress-current progress) new-value)))
