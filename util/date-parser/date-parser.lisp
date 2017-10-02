;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; date-parser.lisp

(defpackage #:raptor-launcher/date-parser
  (:use
   #:cl
   #:esrap
   #:parser.common-rules)
  (:export
   #:*months*
   #:parse-date))

(in-package #:raptor-launcher/date-parser)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *months*
    '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
      "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")))

(defrule skippable whitespace+)

(defrule skippable? whitespace*)

(defrule/s delimiter-comma #\, (:constant nil))

(defrule/s month #.`(or ,@*months*))

(defrule/s day integer-literal/decimal)

(defrule year integer-literal/decimal)

(defrule date (and month/s day/?s delimiter-comma/?s year)
  (:destructure (month day comma year)
    (declare (ignore comma))
    (list day month year)))

(defun parse-date (string)
  (let ((date (esrap:parse 'date string)))
    (setf (second date) (1+ (position (second date) *months* :test #'string=)))
    date))
