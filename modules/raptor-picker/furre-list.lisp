;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; furre-list.lisp

(in-package :raptor-launcher/raptor-picker)
(in-readtable :qtools)

(defvar *character-image-empty* "No Character Image
150x400+
\(click here to add)")

(define-subwidget (raptor-picker image)
    (q+:make-qpushbutton *character-image-empty*)
  (q+:add-widget layout image)
  (setf (q+:minimum-width image) 150
        (q+:maximum-width image) 150
        (q+:flat image) t
        (q+:size-policy image) (values (q+:qsizepolicy.expanding)
                                       (q+:qsizepolicy.expanding))))

(define-subwidget (raptor-picker furre-list) (q+:make-qtablewidget 0 3)
  (q+:add-widget layout furre-list)
  (setf (q+:horizontal-header-labels furre-list)
        '("" "Furre Name" "Last Login")
        (q+:column-width furre-list 0) 24
        (q+:column-width furre-list 1) 180
        (q+:vertical-scroll-mode furre-list)
        (q+:qabstractitemview.scroll-per-pixel)
        (q+:horizontal-scroll-mode furre-list)
        (q+:qabstractitemview.scroll-per-pixel)
        (q+:selection-behavior furre-list)
        (q+:qabstractitemview.select-rows))
  (let ((size-policy (q+:size-policy furre-list)))
    (setf (q+:horizontal-policy size-policy)
          (q+:qsizepolicy.fixed)))
  (let ((h-header (q+:horizontal-header furre-list))
        (v-header (q+:vertical-header furre-list)))
    (setf (q+:stretch-last-section h-header) t
          (q+:resize-mode v-header) (q+:qheaderview.fixed)
          (q+:resize-mode h-header) (q+:qheaderview.resize-to-contents)
          (q+:default-section-size v-header) 24
          (q+:sort-indicator h-header 0) (q+:qt.ascending-order))
    (q+:hide v-header))
  (setf (q+:sorting-enabled furre-list) t))

(defun insert-row (widget s1 s2 s3)
  (let ((count (q+:row-count widget)))
    (q+:insert-row widget count)
    (setf (table-text widget count 0) s1
          (table-text widget count 1) s2
          (table-text widget count 2) s3)))
