;;;; util.lisp

(in-package :furcadia-launcher)

(defun cat (&rest strings)
  (apply #'concatenate 'string strings))

(defun trim-whitespace (string)
  (let ((whitespace '(#\Space #\Newline #\Backspace #\Tab
                      #\Linefeed #\Page #\Return #\Rubout)))
    (string-trim whitespace string)))

(defun hexadecimal-string-p (string)
  (let ((chars '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7
                 #\8 #\9 #\a #\b #\c #\d #\e #\f)))
    (loop for char across string
          unless (member char chars) return nil
            finally (return t))))
