;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; advanced-config.lisp

(in-package :raptor-launcher/raptor-config)
(in-readtable :qtools)

(define-subwidget (raptor-config advanced-config) (q+:make-qwidget)
  (q+:add-tab config-tabs advanced-config "Advanced"))

(define-subwidget (raptor-config advanced-config-layout) (q+:make-qgridlayout)
  (setf (q+:layout advanced-config) advanced-config-layout
        (q+:column-stretch advanced-config-layout 0) 300
        (q+:column-stretch advanced-config-layout 1) 100
        (q+:column-stretch advanced-config-layout 2) 1))

(define-subwidget (raptor-config advanced-config-entry-key)
    (q+:make-qlineedit)
  (q+:add-widget advanced-config-layout advanced-config-entry-key 0 0)
  (setf (q+:placeholder-text advanced-config-entry-key) "Configuration path"))

(define-subwidget (raptor-config advanced-config-entry-value)
    (q+:make-qlineedit)
  (q+:add-widget advanced-config-layout advanced-config-entry-value 0 1)
  (setf (q+:placeholder-text advanced-config-entry-value) "Value"))

(define-subwidget (raptor-config advanced-config-set-button)
    (q+:make-qpushbutton "Set")
  (q+:add-widget advanced-config-layout advanced-config-set-button 0 2))

(define-subwidget (raptor-config advanced-config-table)
    (q+:make-qtablewidget 0 2)
  (q+:add-widget advanced-config-layout advanced-config-table 1 0 1 3)
  (q+:hide (q+:horizontal-header advanced-config-table))
  (setf (q+:column-width advanced-config-table 0) 300
        (q+:vertical-scroll-mode advanced-config-table)
        (q+:qabstractitemview.scroll-per-pixel)
        (q+:horizontal-scroll-mode advanced-config-table)
        (q+:qabstractitemview.scroll-per-pixel)
        (q+:selection-mode advanced-config-table)
        (q+:qabstractitemview.single-selection)
        (q+:selection-behavior advanced-config-table)
        (q+:qabstractitemview.select-rows)
        (q+:horizontal-header-labels advanced-config-table) '("Key" "Value"))
  (let ((h-header (q+:horizontal-header advanced-config-table))
        (v-header (q+:vertical-header advanced-config-table)))
    (setf (q+:stretch-last-section h-header) t
          (q+:resize-mode v-header) (q+:qheaderview.fixed)
          (q+:default-section-size v-header) 20
          (q+:sort-indicator h-header 0) (q+:qt.ascending-order))
    (q+:show h-header)
    (q+:hide v-header))
  (redisplay-config advanced-config-table))

(defun redisplay-config (advanced-config-table &optional config)
  (setf (q+:row-count advanced-config-table) 0)
  (let ((alist (config-alist config))
        (*print-readably* t))
    (note t :trace "Redisplaying ~D configuration lines." (length alist))
    (loop for (key . value) in alist
          for i from 0
          for string-key = (format nil "~{~S~^ ~}" key)
          for string-value = (if (eq (lastcar key) :password)
                                 (witty-password) (prin1-to-string value))
          do (q+:insert-row advanced-config-table i)
             (with-signals-blocked (advanced-config-table)
               (setf (table-text advanced-config-table i 0) string-key
                     (table-text advanced-config-table i 1) string-value)))))

(define-slot (raptor-config tab-clicked) ((tab int))
  (declare (connected config-tabs (current-changed int)))
  ;; TODO turn this into a generic function
  (when (= tab 1) (redisplay-config advanced-config-table))
  (when (= tab 0) (refresh-simple-config raptor-config)))

(define-slot (raptor-config selector-clicked) ()
  (declare (connected selector (clicked)))
  (let ((tab (q+:current-index config-tabs)))
    ;; TODO turn this into a generic function
    (when (= tab 0) (refresh-simple-config raptor-config))
    (when (= tab 1) (redisplay-config advanced-config-table))))

(defparameter *config-set-error*
  "Error of type ~S while setting the config value ~S for path ~S provided ~
in the editor: ~A")

(define-slot (raptor-config update-config) ()
  (declare (connected advanced-config-set-button (clicked)))
  (let* ((key (q+:text advanced-config-entry-key))
         (value (q+:text advanced-config-entry-value)))
    (handler-case
        (let* ((path (read-from-string (uiop:strcat "(" key ")"))))
          (if (string/= "" value)
              (let* ((new-value (read-from-string value))
                     (log-value (if (eq (lastcar path) :password)
                                    (witty-password) new-value)))
                (apply #'(setf config) new-value path)
                (note t :trace "Set config path ~S to value ~S."
                      path log-value))
              (progn
                (apply #'remconfig path)
                (note t :trace "Removed the value from config path ~S."
                      path))))
      (error (e)
        (note t :error *config-set-error* (type-of e) value key e)))
    (redisplay-config advanced-config-table)))

(define-slot (raptor-config set-config) ()
  (declare (connected advanced-config-table (item-selection-changed)))
  (when-let* ((model (q+:selection-model advanced-config-table))
              (rows (q+:selected-rows model)))
    (let* ((row (q+:row (first rows)))
           (key (q+:text (q+:item advanced-config-table row 0)))
           (value (q+:text (q+:item advanced-config-table row 1))))
      (setf (q+:text advanced-config-entry-key) key
            (q+:text advanced-config-entry-value) value))))
