;;;; furcadia-launcher.asd

(asdf:defsystem #:furcadia-launcher
  :description "A launcher for Furcadia"
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "BSD 2-clause"
  :depends-on (:ironclad
               :drakma
               :cl-json)
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "backend")
               (:file "config")
               (:file "ui")))
