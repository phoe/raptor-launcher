;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; module.lisp

(in-package :raptor-launcher/raptor-picker)
(in-readtable :qtools)

(define-signal (raptor-picker show-account-checkbox-clicked) (bool))

(define-slot (raptor-picker show-hide-account-number) ((showp bool))
  (declare (connected raptor-picker (show-account-checkbox-clicked bool)))
  (setf (q+:column-hidden furre-list 0) (not showp)))

(define-signal (raptor-picker last-login-checkbox-clicked) (bool))

(define-slot (raptor-picker last-login-last-login) ((showp bool))
  (declare (connected raptor-picker (last-login-checkbox-clicked bool)))
  (setf (q+:column-hidden furre-list 2) (not showp)))
