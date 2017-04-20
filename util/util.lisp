;;;; util.lisp

(in-package :furcadia-launcher)

(defun cat (&rest strings)
  "Concatenates all strings passed as arguments."
  (apply #'concatenate 'string strings))

(defun trim-whitespace (string)
  "Trims whitespace characters from both sides of a string."
  (let ((whitespace '(#\Space #\Newline #\Backspace #\Tab
                      #\Linefeed #\Page #\Return #\Rubout)))
    (string-trim whitespace string)))

(defun hexadecimal-string-p (string)
  "Returns true if the string contains only digits 0-9 and lowercase characters
a-f, false otherwise."
  (let ((chars '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7
                 #\8 #\9 #\a #\b #\c #\d #\e #\f)))
    (loop for char across string
          unless (member char chars) return nil
            finally (return t))))
