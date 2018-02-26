;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; raptor-launcher.raptor-fusion.asd

(asdf:defsystem #:raptor-launcher.raptor-fusion
  :description "Standard fusion for Raptor Launcher"
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "GPLv3"
  :serial t
  :depends-on
  (#:plump
   #:local-time
   #:raptor-launcher.util
   #:raptor-launcher.protocol
   #:raptor-launcher.config
   #:raptor-launcher.base)
  :components
  ((:file "package")
   (:file "module")))
