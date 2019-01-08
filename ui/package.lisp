;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; package.lisp

(defpackage :raptor-launcher/ui
  (:use #:cl+qt
        #:alexandria)
  (:export #:loading-screen #:reset #:maximum #:minimum #:current))
