;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; definition.lisp

(in-package :raptor-launcher/raptor-picker)
(in-readtable :qtools)

(define-raptor-module raptor-picker (picker)
  (:main-window qwidget qhboxlayout
                (%queue :accessor queue
                        :initform (make-queue))
                (%queue-joiner :accessor queue-joiner
                               :initform nil)
                (%lock :accessor lock
                       :initform (make-lock))
                (%emails-cookie-jars :accessor emails-cookie-jars
                                     :initform (make-hash-table :test #'equal))
                (%emails-accounts :accessor emails-accounts
                                  :initform (make-hash-table :test #'equal)))
  (:selector "Characters")
  (:button play-button "Play!")
  (:button sync-button "Sync")
  (:constructor
      (note t :debug "Raptor Picker starting.")
      (push (set-main-window-for-threads raptor-picker)
       (post-init-callbacks raptor-picker))
      (setf (config-widget-constructor raptor-picker)
            (lambda () (make-instance 'config-widget :module raptor-picker)))))

(defun set-main-window-for-threads (raptor-picker)
  (lambda ()
    (setf (assoc-value *default-special-bindings* '*main-window*)
          (main-window raptor-picker))))
