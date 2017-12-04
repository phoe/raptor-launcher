;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; package.lisp

(defpackage :raptor-launcher/test
  (:use :cl))

(in-package :raptor-launcher/config)

(defun test-config ()
  (uiop:with-temporary-file (:pathname pathname)
    (let ((*storage-pathname* pathname))
      (assert (equal (multiple-value-list (config :foo)) '(nil nil)))
      (setf (config :foo) :bar)
      (assert (equal (multiple-value-list (config :foo)) '(:bar t)))
      (remconfig :foo)
      (assert (equal (multiple-value-list (config :foo)) '(nil nil))))))
