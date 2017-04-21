;;;; worker.lisp

(in-package :furcadia-launcher)

(defvar *kernel*
  (make-kernel 5 :name "Furcadia Launcher kernel")
  "LParallel kernel for the Furcadia launcher.")
