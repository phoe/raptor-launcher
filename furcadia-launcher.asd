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
               :qtools-evaluator
               :qtcore
               :fare-csv
               :split-sequence
               :esrap
               :parser.common-rules
               :qtgui)
  :serial t
  :components ((:file "package")
               (:file "util/util")
               (:file "util/date-parser")
               (:file "logger/logger")
               (:file "db/config")
               (:file "db/state")
               (:file "backend/backend-login")
               (:file "backend/backend-fured")
               (:file "backend/backend-furcadia")
               (:file "news/news")
               (:file "worker/worker-general")
               (:file "worker/worker-login")
               (:file "worker/worker-fured")
               (:file "worker/worker-furcadia")
               (:file "algorithm/algorithm")
               (:file "gui/gui")
               (:file "gui/gui-new")
               )
  :defsystem-depends-on (:qtools)
  :build-operation "qt-program-op"
  :build-pathname "launcher"
  :entry-point "furcadia-launcher-gui:build-main"
  )
