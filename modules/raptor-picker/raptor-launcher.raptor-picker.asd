;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; raptor-launcher.raptor-picker.asd

(asdf:defsystem #:raptor-launcher.raptor-picker
  :description "Standard character picker for Raptor Launcher"
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "GPLv3"
  :serial t
  :depends-on
  (#:cl-furcadia
   #:lparallel
   #:bordeaux-threads
   #:closer-mop
   #:bordeaux-threads
   #:raptor-launcher.util
   #:raptor-launcher.protocol
   #:raptor-launcher.config
   #:raptor-launcher.base)
  :components
  ((:file "package")
   (:file "definition")
   (:file "furre-list")
   (:file "backend")
   (:file "config-widget")
   (:file "loading-screen")
   (:file "module")))
