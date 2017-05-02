;;;; buttons.lisp

(in-package :furcadia-launcher-gui)
(in-readtable :qtools)

;;;; BUTTONS ABOVE

(defmacro launcher-hide-all ()
  `(mapc #'q+:hide
         (list news-box
               config-box
               chars-box
               editor-box
               debug-box
               help-box)))

(defmacro define-launcher-button ((button-name button-text) &body body)
  (flet ((button-name-pressed (button-name)
           (intern (concatenate 'string (symbol-name button-name) "-PRESSED"))))
    `(progn
       (define-subwidget (launcher ,button-name)
           (q+:make-qpushbutton ,button-text)
         (setf (q+:maximum-width ,button-name) 90
               (q+:minimum-width ,button-name) 90
               (q+:auto-default ,button-name) nil))
       (define-slot (launcher ,(button-name-pressed button-name)) ()
         (declare (connected ,button-name (clicked)))
         ,@body))))

(define-launcher-button (button-news "News")
    (launcher-hide-all)
  (q+:show news-box))

(define-launcher-button (button-config "Config")
    (launcher-hide-all)
  (q+:show config-box))

(define-launcher-button (button-chars "Characters")
    (launcher-hide-all)
  (q+:show chars-box))

(define-launcher-button (button-editor "Editor")
    (launcher-hide-all)
  (q+:show editor-box))

(define-launcher-button (button-debug "Debug")
    (launcher-hide-all)
  (q+:show debug-box))

(define-launcher-button (button-help "Help")
    (launcher-hide-all)
  (q+:show help-box))

;;;; TODO RaptorLauncher image label here

;;;; BUTTONS BELOW

(define-launcher-button (button-play "Play!")
    (setf (q+:default button-play) t))

(define-launcher-button (button-sync "Sync!"))

(define-launcher-button (button-quit "Quit")
  (q+:close launcher))

(define-slot (launcher news-fetched) ()
  (declare (connected launcher (enable-news)))
  (let* ((news furcadia-launcher::*fetched-news*)
         (updatedp furcadia-launcher::*fetched-updatedp*)
         (widgets (mapcar #'make-news-widget news)))
    (mapc (curry #'q+:add-widget news-contents) widgets)
    (setf (q+:enabled button-news) t)
    (when updatedp
      (let ((font (q+:make-qfont)))
        (setf (q+:bold font) t
              (q+:text button-news) "News!"
              (q+:font button-news) font)))))
