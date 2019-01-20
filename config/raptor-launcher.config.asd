;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; raptor-launcher.config.asd

(asdf:defsystem #:raptor-launcher.config
  :description "Configuration for Raptor Launcher"
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "GPLv3"
  :serial t
  :depends-on (#:alexandria
               #:ubiquitous-concurrent
               #:cl-furcadia.protocol
               #:cl-furcadia.clos
               #:raptor-launcher.protocol)
  :components ((:file "package")
               (:file "config")
               (:file "store-restore")))
