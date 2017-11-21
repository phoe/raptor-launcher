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
               ;; protocol
               #:protest
               ;; Qt
               #:qtools
               #:qtcore
               #:qtgui
               ;; Raptor Launcher
               #:raptor-launcher.util)
  :components ((:file "package")
               (:file "main-window")
               (:file "modules")
               (:file "test")))
