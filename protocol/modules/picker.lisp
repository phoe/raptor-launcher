;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; characters.lisp

(in-package :raptor-launcher/protocol)

(define-protocol picker
    (:description "The PICKER protocol describes Raptor Launcher modules that ~
are used for selecting, viewing and launching individual Furcadia characters."
     :tags (:raptor-launcher :module :picker)
     :export t)
  (:function sync ((picker picker)) (values))
  ""
  (:function sname-character ((picker picker) (sname string)) (furre furre))
  ""
  (:function characters ((picker picker)) t)
  ""
  (:function select-character ((picker picker) (sname string)) (values))
  ""
  (:function select-character-and-switch
             ((picker picker) (sname string)) (values))
  "")
