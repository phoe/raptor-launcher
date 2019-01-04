;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; framework.lisp

(in-package :raptor-launcher/raptor-picker)
(in-readtable :qtools)

;; (petri::display-graph
;;  (petri-net ()
;;    (credentials -> #'login -> cookie-jars
;;                 -> #'dl-account
;;                 -> accounts accounts-images accounts-furres)
;;    (accounts-images -> #'dl-images -> (images *))
;;    (accounts-furres -> #'dl-furres
;;                     -> (furres *)
;;                     (furres-costumes *)
;;                     (furres-portraits *)
;;                     (furres-specitags *))
;;    (furres-costumes -> #'dl-costumes -> costumes)
;;    (furres-portraits -> #'dl-portraits -> portraits)
;;    (furres-specitags -> #'dl-specitags -> specitags)))
