;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; raptor-launcher.raptor-config.asd

(asdf:defsystem #:raptor-launcher.raptor-config
  :description "Configuration manager for Raptor Launcher"
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "GPLv3"
  :serial t
  :depends-on
  (#:plump
   #:local-time
   #:split-sequence
   #:raptor-launcher.util
   #:raptor-launcher.protocol
   #:raptor-launcher.base)
  :components
  ((:file "package")
   (:file "definition")
   (:file "config-widget")
   (:file "simple-config")
   (:file "advanced-config")
   (:file "tabs-callbacks")))
