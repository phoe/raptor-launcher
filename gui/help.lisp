;;;; help.lisp

(in-package :furcadia-launcher-gui)
(in-readtable :qtools)

(defparameter *help-box-text*
  (format nil "<h3>I found a bug!/How can I get help?</h3>
<ol>
  <li> <b><u>Do not ask the Furcadia team for help!</u></b><br>
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

(define-subwidget (launcher help-box-contents)
    (q+:make-qlabel *help-box-text*)
  (setf (q+:word-wrap help-box-contents) t
        (q+:text-interaction-flags help-box-contents)
        (q+:qt.text-selectable-by-mouse)
        (q+:text-interaction-flags help-box-contents)
        (q+:qt.text-browser-interaction)
        (q+:open-external-links help-box-contents) t))

(define-subwidget (launcher help-box) (q+:make-qscrollarea)
  (setf (q+:widget help-box) help-box-contents
        (q+:widget-resizable help-box) t
        (q+:frame-shape help-box) (q+:qframe.no-frame)))
