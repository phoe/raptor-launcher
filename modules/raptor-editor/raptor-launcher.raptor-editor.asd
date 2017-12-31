;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; raptor-launcher.raptor-editor.asd

(asdf:defsystem #:raptor-launcher.raptor-editor
  :description "Furre editor for Raptor Launcher"
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "GPLv3"
  :serial t
  :depends-on
  (#:alexandria
   #:protest
   #:static-vectors
   #:cl-furcadia.constants
   #:raptor-launcher.util
   #:raptor-launcher.protocol
   #:raptor-launcher.config
   #:raptor-launcher.base)
  :components
  ((:file "package")
   (:file "definition")
   (:file "layout")
   (:file "description")
   (:file "looks")
   (:file "palette/palette")
   (:file "palette/color-palette")
   (:file "palette/color-picker")))
