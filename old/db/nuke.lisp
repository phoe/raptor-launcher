;;;; nuke.lisp

(in-package :furcadia-launcher)

(defun nuke-whole-config ()
  "Cleans all configuration files on disk."
  (let (*config* (*state* (make-hash-table)) (*state-lock* (make-lock)))
    (save-config-file)
    (save-state-file)))
