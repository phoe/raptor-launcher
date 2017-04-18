;;;; util.lisp

(in-package :furcadia-launcher)

(defun cat (&rest strings)
  (apply #'concatenate 'string strings))

(defun trim-whitespace (string)
  (let ((whitespace '(#\Space #\Newline #\Backspace #\Tab
                      #\Linefeed #\Page #\Return #\Rubout)))
    (string-trim whitespace string)))
