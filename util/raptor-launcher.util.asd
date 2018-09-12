;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; raptor-launcher.util.asd

(asdf:defsystem #:raptor-launcher.util
  :description "Utilities and common files for Raptor Launcher"
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "GPLv3"
  :serial t
  :depends-on
  (#:alexandria
   #:phoe-toolbox
   #:qtools)
  :components
  ((:file "package")
   (:file "util")
   (:file "witty-lines")))
