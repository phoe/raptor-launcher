;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; raptor-launcher.protocol.asd

(asdf:defsystem #:raptor-launcher.protocol
  :description "Protocols for Raptor Launcher"
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "GPLv3"
  :serial t
  :depends-on (#:closer-mop
               #:moptilities
               #:alexandria
               #:protest)
  :components ((:file "package")
               ;; base
               (:file "module")
               (:file "main-window")
               ;; modules
               (:file "modules/logger")
               (:file "modules/picker")
               (:file "modules/config")))
