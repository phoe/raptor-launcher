;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; module.lisp

(in-package :raptor-launcher/raptor-logger)
(in-readtable :qtools)

(define-raptor-module raptor-config (module) ;; TODO create protoclass CONFIG
  (:main-window qwidget qhboxlayout
                (log-list :accessor log-list :initform '()))
  (:selector "Config")
  (:constructor
      (note t :debug "Raptor Config starting.")))

(define-subwidget (raptor-config config-list) (q+:make-qtablewidget 0 2)
  (q+:add-widget layout config-list)
  (setf (q+:horizontal-header-labels config-list) '("Key" "Value")
        (q+:column-width config-list 0) 300
        (q+:vertical-scroll-mode config-list)
        (q+:qabstractitemview.scroll-per-pixel)
        (q+:horizontal-scroll-mode config-list)
        (q+:qabstractitemview.scroll-per-pixel))
  (let ((h-header (q+:horizontal-header config-list))
        (v-header (q+:vertical-header config-list)))
    (setf (q+:stretch-last-section h-header) t
          (q+:resize-mode v-header) (q+:qheaderview.fixed)
          (q+:default-section-size v-header) 20
          (q+:sort-indicator h-header 0) (q+:qt.ascending-order))
    (q+:hide v-header))
  (redisplay-config config-list))

(defvar *redisplay-config-fire-callback* t)

(defun redisplay-config (config-list &optional config)
  (setf (q+:row-count config-list) 0)
  (let ((alist (config-alist config))
        (*print-readably* t))
    (note t :trace "Redisplaying ~D configuration lines." (length alist))
    (loop for (key . value) in alist
          for i from 0
          for string-key = (format nil "~{~S~^ ~}" key)
          for string-value = (if (eq (lastcar key) :password)
                                 "hunter2"
                                 (prin1-to-string value))
          do (q+:insert-row config-list i)
             (setf (table-text config-list i 0) string-key
                   (table-text config-list i 1 t) string-value))))

(defvar *config-set-error*
  "Error of type ~S while setting the new config value ~S for path ~S provided ~
in the editor: ~A")

(define-slot (raptor-config update-config) ((item "QTableWidgetItem *" t))
  (declare (connected config-list (item-changed "QTableWidgetItem *")))
  (when *redisplay-config-fire-callback*
    (let ((*redisplay-config-fire-callback* nil))
      (let* ((row (q+:row item))
             (text (q+:text item))
             (key (q+:text (q+:item config-list row 0)))
             (path (read-from-string (uiop:strcat "(" key ")")))
             (old-value (apply #'config path)))
        (handler-case
            (let* ((new-value (read-from-string text))
                   (log-value (if (eq (lastcar key) :password)
                                  "hunter2" new-value)))
              (apply #'(setf config) new-value path)
              (note t :trace "Set config path ~S to value ~S." path log-value))
          (error (e)
            (note t :error *config-set-error* (type-of e) text path e)
            (apply #'(setf config) old-value path)))
        (redisplay-config config-list)))))

(defun (setf table-text) (new-value widget row column &optional editablep)
  (when (null-qobject-p (q+:item widget row column))
    (setf (q+:item widget row column) (q+:make-qtablewidgetitem)))
  (let ((item (q+:item widget row column)))
    (setf (q+:text item) new-value
          (q+:flags item) (+ (q+:qt.item-is-selectable)
                             (q+:qt.item-is-enabled)
                             (if editablep (q+:qt.item-is-editable) 0)))))
