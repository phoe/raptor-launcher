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

(defun sleepcar (function list &optional (n 100) (sleep-interval 1))
  "Collects the element of funcalling FUNCTION on successive elements of LIST,
sleeping for SLEEP-INTERVAL seconds every N elements."
  (loop for elt in list
        for i from 1
        collect (funcall function elt)
        when (zerop (mod i n))
          do (sleep sleep-interval)))

(defun fformat (stream format-string &rest format-args)
  "Acts like FORMAT, except it calls FORCE-OUTPUT on STREAM afterwards."
  (apply #'format stream format-string format-args)
  (force-output stream))

(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
          for old-pos = 0 then (+ pos part-length)
          for pos = (search part string
                            :start2 old-pos
                            :test test)
          do (write-string string out
                           :start old-pos
                           :end (or pos (length string)))
          when pos do (write-string replacement out)
            while pos)))

(defun get-unix-time ()
  "Returns the current Unix timestamp."
  (- (get-universal-time) 2208988800))

(defun unix-time-to-datestring (unix-time)
  "Decodes the unix time and returns its textual form in format
\"YYYY-MM-DD HH:MM:SS\"."
  (let* ((universal-time (+ unix-time 2208988800))
         (time (multiple-value-list (decode-universal-time universal-time))))
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            (nth 5 time) (nth 4 time) (nth 3 time)
            (nth 2 time) (nth 1 time) (nth 0 time))))

(defun keywordize (string)
  "Given a string, upcases it returns a keyword with the upcased name."
  (intern (string-upcase string) :keyword))

(defun keywordize-cars (list)
  "Traverses and destructively modifies the provided list by replacing the CAR
of each list with its keywordized version, if said CAR is a string."
  (when (consp list)
    (when (stringp (car list))
      (setf (car list)
            (keywordize (car list))))
    (mapc #'keywordize-cars list))
  list)

;;;; WITTY LINES

(defparameter *witty-lines*
  '("Bouncing raptors on dragons..."
    "Painting all characters green..."
    "Setting all passwords to qwerty123..."
    "Randomly flipping characters' genders..."
    "Loading up catapults..."
    "Shaving cats..."
    "Obfuscating configuration..."
    "Brewing original British tea on your CPU..."
    "Launching all alts at once..."
    "Launching pies..."
    "Wasting your time..."
    "Ordering higher-quality raptors..."
    "Rewriting launcher code..."
    "Stopping the world..."
    "Loading Windows 98..."
    "Retrieving the mislaunched raptor..."
    "Fetching registry info..."
    "Deleting Furcadia..."
    "Formatting the hard drive..."
    "Setting up hacker backdoors..."
    "Reading floppy disk..."
    "Increasing security by 25%...")
  "These are meant to be funny, you know.")

(defun witty-line ()
  "Returns a random witty line."
  (random-elt *witty-lines*))
