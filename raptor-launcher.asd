;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; raptor-launcher.asd

(asdf:defsystem #:raptor-launcher
  :description "A graphical launcher and toolsuite for Furcadia"
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "GPLv3"
  :serial t
  :depends-on
  (#:raptor-launcher.util
   #:raptor-launcher.protocol
   #:raptor-launcher.config
   #:raptor-launcher.base
   #:raptor-launcher.raptor-logger
   #:raptor-launcher.raptor-config
   #:raptor-launcher.raptor-editor
   #:raptor-launcher.raptor-picker
   #:raptor-launcher.raptor-fusion
   #:raptor-launcher.raptor-chat
   )
  :components
  ((:file "package")))
