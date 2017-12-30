;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; raptor-launcher.base.asd

(asdf:defsystem #:raptor-launcher.base
  :description "Qtools base for Raptor Launcher"
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "GPLv3"
  :serial t
  :depends-on (;; utils
               #:alexandria
               #:phoe-toolbox
               #:static-vectors
               ;; protocol
               #:protest
               ;; Qt
               #:qtools
               #:qtools-ui-flow-layout
               #:qtcore
               #:qtgui
               ;; Raptor Launcher
               #:raptor-launcher.util
               #:raptor-launcher.protocol)
  :components ((:file "package")
               (:file "utils")
               (:file "raptor-launcher")
               (:file "modules")))
