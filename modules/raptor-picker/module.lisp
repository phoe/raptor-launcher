;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; module.lisp

(in-package :raptor-launcher/raptor-picker)
(in-readtable :qtools)

(define-raptor-module raptor-picker (picker)
  (:main-window qwidget qhboxlayout
                (log-list :accessor log-list :initform '())
                (accounts :accessor accounts
                          :initform '())
                (furres :accessor furres
                        :initform '())
                (cookie-jar :accessor cookie-jar
                            :initform (make-instance 'drakma:cookie-jar)))
  (:selector "Characters")
  (:button play-button "Play!")
  (:button sync-button "Sync")
  (:constructor
      (note t :info "Raptor Picker starting.")))
