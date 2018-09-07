;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; raptor-launcher.raptor-chat.asd

(asdf:defsystem #:raptor-launcher.raptor-chat
  :description "Chat window for Raptor Launcher"
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "GPLv3"
  :serial t
  :depends-on
  (#:spell/simple
   #:wordnet
   #:trivial-indent
   #:cl-opengl
   #:qtopengl
   #:raptor-launcher.util
   #:raptor-launcher.protocol
   #:raptor-launcher.base)
  :components
  ((:file "package")
   (:file "definition")
   (:file "spellchecked-text-edit")
   (:file "dictionary")
   (:file "image-widget")
   (:file "chat")))
