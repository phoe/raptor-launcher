;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; config.lisp

(in-package :raptor-launcher/protocol)

(define-protocol config
    (:description "The CONFIG protocol describes Raptor Launcher modules that ~
are meant for allowing the user to read and modify the Launcher's
configuration."
     :tags (:raptor-launcher :module :config)
     :export t)
  (:class config (module) ())
  "A config object. Each class participating in the protocol must subclass ~
this protocol class."
  (:function accounts (config) list)
  "Returns a list of all accounts available in the configuration. The result ~
is a list of two-element lists, where the first element is the email and the ~
second element is the password."
  (:config :config :show-advanced)
  "The configuration value stating if the configuration widget should display ~
a window with advanced options."
  (:config :config :accounts n :email)
  "The configuration value stating the email used for account N."
  (:config :config :accounts n :password)
  "The configuration value stating the password used for account N.")
