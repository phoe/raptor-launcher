;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; advanced-config.lisp

(in-package :raptor-launcher/raptor-config)
(in-readtable :qtools)

(define-subwidget (raptor-config advanced-config) (q+:make-qtablewidget 0 2)
  (q+:add-tab config-tabs advanced-config "Advanced")
  (q+:hide (q+:horizontal-header advanced-config))
  (setf (q+:column-width advanced-config 0) 300
        (q+:vertical-scroll-mode advanced-config)
        (q+:qabstractitemview.scroll-per-pixel)
        (q+:horizontal-scroll-mode advanced-config)
        (q+:qabstractitemview.scroll-per-pixel)
        (q+:selection-behavior advanced-config)
        (q+:qabstractitemview.select-items))
  (let ((h-header (q+:horizontal-header advanced-config))
        (v-header (q+:vertical-header advanced-config)))
    (setf (q+:stretch-last-section h-header) t
          (q+:resize-mode v-header) (q+:qheaderview.fixed)
          (q+:default-section-size v-header) 20
          (q+:sort-indicator h-header 0) (q+:qt.ascending-order))
    (q+:hide v-header))
  (redisplay-config advanced-config))

(defun redisplay-config (advanced-config &optional config)
  (setf (q+:row-count advanced-config) 0)
  (let ((alist (config-alist config))
        (*print-readably* t))
    (note t :trace "Redisplaying ~D configuration lines." (length alist))
    (loop for (key . value) in alist
          for i from 0
          for string-key = (format nil "~{~S~^ ~}" key)
          for string-value = (if (eq (lastcar key) :password)
                                 (witty-password) (prin1-to-string value))
          do (q+:insert-row advanced-config i)
             (with-signals-blocked (advanced-config)
               (setf (table-text advanced-config i 0) string-key
                     (table-text advanced-config i 1 t) string-value)))))

(define-slot (raptor-config tab-clicked) ((tab int))
  (declare (connected config-tabs (current-changed int)))
  (when (= tab 1) (redisplay-config advanced-config)))

(define-slot (raptor-config selector-clicked) ()
  (declare (connected selector (clicked)))
  (let ((tab (q+:current-index config-tabs)))
    (when (= tab 1) (redisplay-config advanced-config))))

(defvar *config-set-error*
  "Error of type ~S while setting the new config value ~S for path ~S provided ~
in the editor: ~A")

(define-slot (raptor-config update-config) ((item "QTableWidgetItem *" t))
  (declare (connected advanced-config (item-changed "QTableWidgetItem *")))
  (let* ((row (q+:row item))
         (text (q+:text item))
         (key (q+:text (q+:item advanced-config row 0)))
         (path (read-from-string (uiop:strcat "(" key ")")))
         (old-value (apply #'config path)))
    (handler-case
        (let* ((new-value (read-from-string text))
               (log-value (if (eq (lastcar path) :password)
                              (witty-password) new-value)))
          (apply #'(setf config) new-value path)
          (note t :trace "Set config path ~S to value ~S." path log-value))
      (error (e)
        (note t :error *config-set-error* (type-of e) text path e)
        (apply #'(setf config) old-value path)))
    (redisplay-config advanced-config)))

(defun (setf table-text) (new-value widget row column &optional editablep)
  (when (null-qobject-p (q+:item widget row column))
    (setf (q+:item widget row column) (q+:make-qtablewidgetitem)))
  (let ((item (q+:item widget row column)))
    (setf (q+:text item) new-value
          (q+:flags item) (+ (q+:qt.item-is-selectable)
                             (q+:qt.item-is-enabled)
                             (if editablep (q+:qt.item-is-editable) 0)))))
