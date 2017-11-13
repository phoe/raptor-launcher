;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; phoe-toolbox.lisp

(in-package :raptor-launcher/util)

;;; Common utilities depend on the PHOE-TOOLBOX system.

(defun keywordize-cars (list) ;; TODO do we use this at all?
  "Traverses and destructively modifies the provided list by replacing the CAR
of each list with its keywordized version, if said CAR is a string."
  (when (consp list)
    (when (stringp (car list))
      (setf (car list)
            (keywordize (car list))))
    (mapc #'keywordize-cars list))
  list)
