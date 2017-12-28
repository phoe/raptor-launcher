;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; module.lisp

(in-package :raptor-launcher/raptor-logger)
(in-readtable :qtools)

(define-raptor-module raptor-config (config)
  (:main-window qwidget qhboxlayout
                (log-list :accessor log-list :initform '()))
  (:selector "Config")
  (:constructor
      (note t :debug "Raptor Config starting.")
      (setf (config-widget-constructor raptor-config)
       (lambda () (make-instance 'config-widget :raptor-config raptor-config)))
      (push (make-callback raptor-config)
            (post-init-callbacks raptor-config))))

;;; Configuration widget

;; TODO config-widget protoclass
(define-widget config-widget (qwidget)
  ((raptor-config :accessor raptor-config
                  :initarg :raptor-config)))

(define-qt-constructor (config-widget)
  (let ((checkedp (config :config :show-advanced)))
    (signal! (raptor-config config-widget)
             (advanced-checkbox-clicked bool) checkedp)))

(define-subwidget (config-widget layout) (q+:make-qgridlayout)
  (setf (q+:layout config-widget) layout
        (q+:column-stretch layout 0) 300))

(define-subwidget (config-widget title) (q+:make-qlabel "Configuration")
  (q+:add-widget layout title 0 0 1 1)
  (setf (q+:style-sheet title) "font-weight: bold;"))

(define-subwidget (config-widget separator) (q+:make-qframe)
  (q+:add-widget layout separator 1 0 1 1)
  (setf (q+:frame-shape separator) (q+:qframe.hline)
        (q+:frame-shadow separator) (q+:qframe.sunken)))

(define-subwidget (config-widget checkbox)
    (q+:make-qcheckbox "Show advanced options?")
  (q+:add-widget layout checkbox 2 0 1 1)
  (let* ((checkedp (config :config :show-advanced))
         (state (if checkedp (q+:qt.checked) (q+:qt.unchecked))))
    (setf (q+:check-state checkbox) state)))

(define-subwidget (config-widget spacer) (q+:make-qwidget)
  (q+:add-widget layout spacer 3 0 1 1)
  (setf (q+:row-stretch layout 3) 9001))

(define-slot (config-widget show-advanced) ()
  (declare (connected checkbox (clicked)))
  (let ((checkedp (q+:is-checked checkbox)))
    (setf (config :config :show-advanced) checkedp)
    (signal! (raptor-config config-widget)
             (advanced-checkbox-clicked bool) checkedp)))

;;; Config tabs

(define-subwidget (raptor-config config-tabs) (q+:make-qtabwidget)
  (q+:add-widget layout config-tabs))

(define-signal (raptor-config advanced-checkbox-clicked) (bool))

(define-slot (raptor-config toggle-advanced) ((checkedp bool))
  (declare (connected raptor-config (advanced-checkbox-clicked bool)))
  (let ((index (q+:index-of config-tabs advanced-config)))
    (if checkedp
        (q+:add-tab config-tabs advanced-config "Advanced")
        (q+:remove-tab config-tabs index))))

;;; Simple config ;; TODO extract these to separate files

(define-subwidget (raptor-config simple-config) (q+:make-qwidget)
  (q+:add-tab config-tabs simple-config "Simple"))

(define-subwidget (raptor-config scroll-layout) (q+:make-qgridlayout)
  (setf (q+:layout simple-config) scroll-layout
        (q+:contents-margins scroll-layout) (values 0 0 0 0)))

(define-subwidget (raptor-config scroll) (q+:make-qscrollarea)
  (q+:add-widget scroll-layout scroll 0 0 1 1)
  (setf (q+:widget-resizable scroll) t
        (q+:frame-shape scroll) (q+:qframe.no-frame)))

(define-subwidget (raptor-config contents) (q+:make-qwidget)
  (setf (q+:widget scroll) contents))

(define-subwidget (raptor-config simple-config-layout) (q+:make-qvboxlayout)
  (setf (q+:layout contents) simple-config-layout
        (q+:contents-margins simple-config-layout) (values 0 0 0 0)))

;;; Advanced config

(define-subwidget (raptor-config advanced-config) (q+:make-qtablewidget 0 2)
  (q+:add-tab config-tabs advanced-config "Advanced")
  (setf (q+:horizontal-header-labels advanced-config) '("Key" "Value")
        (q+:column-width advanced-config 0) 300
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
        (*print-readably* nil))
    (note t :trace "Redisplaying ~D configuration lines." (length alist))
    (loop for (key . value) in alist
          for i from 0
          for string-key = (format nil "~{~S~^ ~}" key)
          for string-value = (if (eq (lastcar key) :password)
                                 "hunter2"
                                 (prin1-to-string value))
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
                              "hunter2" new-value)))
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

(defun make-callback (raptor-config)
  (with-all-slots-bound (raptor-config raptor-config)
    (lambda ()
      (let* ((modules (loaded-modules (main-window raptor-config)))
             (constructors (mapcar #'config-widget-constructor modules))
             (widgets (mapcar #'funcall (remove nil constructors))))
        (mapc (lambda (x) (q+:add-widget simple-config-layout x))
              widgets)))))
