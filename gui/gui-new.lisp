;;;; gui-new.lisp

(in-package :furcadia-launcher-gui)

;;;; MACROS

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

;;;; LAUNCHER

(define-widget launcher (QMainWindow) ())

(define-subwidget (launcher central-widget) (q+:make-qwidget)
  (setf (q+:central-widget launcher) central-widget))

(define-subwidget (launcher image) (q+:make-qpushbutton "No Character Image
\(click here to add)
\(150x400)")
  (setf (q+:minimum-width image) 150
        (q+:maximum-width image) 150
        (q+:flat image) t
        (q+:size-policy image) (values (q+:qsizepolicy.expanding)
                                       (q+:qsizepolicy.expanding))))

;;;; CENTERS

(define-subwidget (launcher news-box) (q+:make-qlabel))

(define-subwidget (launcher config-box-save) (q+:make-qpushbutton "Save")
  (setf (q+:maximum-width config-box-save) 90
        (q+:minimum-width config-box-save) 90))
(define-subwidget (launcher config-box-layout) (q+:make-qgridlayout)
  (setf (q+:contents-margins config-box-layout) (values 0 0 0 0))
  (q+:add-widget config-box-layout config-box-save)
  (q+:add-widget config-box-layout (q+:make-qlabel "Path to Furcadia"))
  (q+:add-widget config-box-layout (q+:make-qlabel "Accounts")))
(define-subwidget (launcher config-box-contents) (q+:make-qwidget))
(define-subwidget (launcher config-box) (q+:make-qscrollarea)
  (setf (q+:layout config-box) config-box-layout
        (q+:widget config-box) config-box-contents
        (q+:widget-resizable config-box) t
        (q+:frame-shape config-box) (q+:qframe.no-frame)))

(progn ;;chars-box
  (define-subwidget (launcher character-list) (q+:make-qtablewidget)
    (setf (q+:minimum-height character-list) 100))
  (define-subwidget (launcher description-preview) (q+:make-qtextedit)
    (setf (q+:minimum-height description-preview) 100))
  (define-subwidget (launcher chars-box) (q+:make-qsplitter)
    (q+:add-widget chars-box character-list)
    (q+:add-widget chars-box description-preview)
    (setf (q+:orientation chars-box) (q+:qt.vertical)
          (q+:children-collapsible chars-box) nil
          (q+:stretch-factor chars-box) (values 0 2)
          (q+:stretch-factor chars-box) (values 1 1))))

(define-subwidget (launcher editor-box) (q+:make-qwidget))

(progn ;;debug-box
  (defparameter *debug-intro*
    "; This is the debug read-eval-print-loop of the Common Lisp image running ~
underneath the launcher. Warning: typing stuff here CAN MAKE YOUR LAUNCHER ~
EXPLODE. If you are sure that you want to use it - delete this comment and ~
happy hacking!")
  (define-subwidget (launcher debug-logs) (q+:make-qtextbrowser))
  (define-subwidget (launcher debug-box) (q+:make-qsplitter)
    (q+:add-widget debug-box debug-logs)
    (let* ((evaluator (make-instance 'qtools-evaluator::evaluator))
           (layout (find-child evaluator (find-qclass "QVBoxLayout")))
           (repl (find-child evaluator (find-qclass "QTextEdit")))
           (stream (qtools-evaluator::repl-output-stream evaluator))
           (font (q+:make-qfont "Monospace" 7 50 nil)))
      (q+:add-widget debug-box evaluator)
      (setf (q+:contents-margins layout) (values 0 0 0 0)
            (q+:style-hint font) (q+:qfont.type-writer)
            (q+:font repl) font
            (q+:orientation debug-box) (q+:qt.vertical)
            (q+:children-collapsible debug-box) nil
            (q+:stretch-factor debug-box) (values 0 5)
            (q+:stretch-factor debug-box) (values 1 1))
      (format stream *debug-intro*))))

(progn ;;help-box
  (defparameter *help-box-text*
    (format nil "<h3>I found a bug!/How can I get help?</h3>
<ol>
  <li><u>Do not ask the Furcadia team for help!</u><br>
This is a third-party launcher, written by a person outside the Furcadia team.
The people over at Catnip Studios do not have the knowledge or resources
to properly support it. Asking them for help will only have you redirected
back to me, and waste a while of their precious supporting time.</li>
  <li>Contact me directly, using the contact info on the bottom of this page.
</li>
  <li>You can see if your issue is already listed on the
<a href=\"https://github.com/phoe/furcadia-launcher/issues\">GitHub issues
page</a> for the launcher. If not, then, if you have a <b>GitHub</b> account,
you can post an issue yourself.</li>
  <li>If you know any other users of this launcher, check if they experience
the same behaviour as you. This will be important in the debugging process.</li>
</ol>

<h3>Credits and licensing</h3>
<b>Furcadia:</b> Aza d'Orano<br />
<b>Email:</b> <b><tt>phoe@openmailbox.org</tt></b><br />
<b>Github:</b> <a href=\"https://github.com/phoe/\">phoe</a><br /><br />
© 2017 Michał \"phoe\" Herda.<br /><br />
<small>This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3 of the License, or (at your option) any
later version.</small>
"))
  (define-subwidget (launcher help-box-contents) (q+:make-qlabel *help-box-text*)
    (setf (q+:word-wrap help-box-contents) t
          ;; myLabel->setTextInteractionFlags(Qt::TextBrowserInteraction);
          (q+:text-interaction-flags help-box-contents)
          (q+:qt.text-selectable-by-mouse)
          (q+:text-interaction-flags help-box-contents)
          (q+:qt.text-browser-interaction)
          (q+:open-external-links help-box-contents) t))
  (define-subwidget (launcher help-box) (q+:make-qscrollarea)
    (setf (q+:widget help-box) help-box-contents
          (q+:widget-resizable help-box) t
          (q+:frame-shape help-box) (q+:qframe.no-frame))))

;;;; BUTTONS ABOVE

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

;;;; BUTTONS BELOW

(define-launcher-button (button-play "Play!")
  (setf (q+:default button-play) t))

(define-launcher-button (button-sync "Sync!"))

(define-launcher-button (button-quit "Quit")
  (q+:close launcher))

;;;; MAIN LAYOUT

(define-subwidget (launcher layout) (q+:make-qgridlayout)
  (setf (q+:window-title launcher) "Launcher"
        (q+:fixed-size launcher) (values 600 420)
        (q+:layout central-widget) layout)
  (mapc (curry #'apply #'q+:add-widget layout)
        `(;;; IMAGE
          (,image 0 0 11 1)
          ;; BOXEN
          (,news-box 0 1 11 1)
          (,config-box 0 1 11 1)
          (,editor-box 0 1 11 1)
          (,debug-box 0 1 11 1)
          (,help-box 0 1 11 1)
          (,chars-box 0 1 11 1)
          ;;; BUTTONS
          (,button-news 0 2)
          (,button-config 1 2)
          (,button-chars 2 2)
          (,button-editor 3 2)
          (,button-debug 4 2)
          (,button-help 5 2)
          (,button-play 8 2)
          (,button-sync 9 2)
          (,button-quit 10 2)))
  (launcher-hide-all)
  (q+:show chars-box)
  (q+:set-focus button-play))
