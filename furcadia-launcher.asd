;;;; furcadia-launcher.asd

(asdf:defsystem #:furcadia-launcher
  :description "A launcher for Furcadia"
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "GPLv3"
  :depends-on (:alexandria
               :ironclad
               :drakma
               :cl-json
               :bordeaux-threads
               :lparallel
               :qtools
               :qtcore
               :qtgui)
  :serial t
  :components ((:file "package")
               (:file "util/util")
               (:file "logger/logger")
               (:file "db/config")
               (:file "db/state")
               (:file "backend/backend-login")
               (:file "backend/backend-fured")
               (:file "backend/backend-furcadia")
               (:file "worker/worker-general")
               (:file "worker/worker-login")
               (:file "worker/worker-fured")
               (:file "worker/worker-furcadia")
               (:file "algorithm/algorithm")
               (:file "gui/gui")
               )
  :defsystem-depends-on (:qtools)
  :build-operation "qt-program-op"
  :build-pathname "launcher"
  :entry-point "furcadia-launcher-gui:build-main"
  )
