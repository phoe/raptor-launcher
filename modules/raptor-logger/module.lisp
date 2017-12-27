;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; module.lisp

(in-package :raptor-launcher/raptor-logger)
(in-readtable :qtools)

(define-raptor-module raptor-logger (logger)
  (:main-window qwidget qvboxlayout
                (log-list :accessor log-list :initform '()))
  (:selector "Logger")
  (:button clear-button "Clear")
  (:constructor
      ;; TODO fix this so all loggers get the information
      (note raptor-logger :debug "Raptor Logger starting.")))

(define-subwidget (raptor-logger debug-logs) (q+:make-qplaintextedit)
  (q+:add-widget layout debug-logs)
  (setf (q+:read-only debug-logs) t))

(define-signal (raptor-logger new-log) (string))

(define-slot (raptor-logger add-log) ((message string))
  (declare (connected raptor-logger (new-log string)))
  (q+:append-html debug-logs message))

(define-slot (raptor-logger clear-logs) ()
  (declare (connected clear-button (pressed)))
  (q+:clear debug-logs)
  (setf (log-list raptor-logger) '())
  (note t :debug "Logs cleared."))

(define-subwidget (raptor-logger log-level-chooser) (q+:make-qwidget)
  (q+:add-widget layout log-level-chooser))

(define-subwidget (raptor-logger log-level-layout) (q+:make-qhboxlayout)
  (setf (q+:layout log-level-chooser) log-level-layout))

(define-subwidget (raptor-logger log-level-label)
    (q+:make-qlabel "Maxuimum log level:")
  (q+:add-widget log-level-layout log-level-label))

(define-subwidget (raptor-logger log-level-dropdown)
    (q+:make-qcombobox)
  (let* ((log-levels (mapcar #'car *log-levels*))
         (strings (mapcar #'string log-levels)))
    (q+:add-widget log-level-layout log-level-dropdown)
    (q+:add-items log-level-dropdown strings))
  (let* ((current-level (string (config :logger :log-level :min-level-shown)))
         (index (q+:find-text log-level-dropdown (string current-level))))
    (q+:set-current-index log-level-dropdown index)))

(define-slot (raptor-logger log-level-changed) ((level string))
  (declare (connected log-level-dropdown (current-index-changed string) level))
  (let ((keyword (make-keyword level)))
    (setf (config :logger :log-level :min-level-shown) keyword)
    (render-all-logs raptor-logger keyword)))

(defmethod render-all-logs ((logger raptor-logger) (level symbol))
  (with-all-slots-bound (logger raptor-logger)
    (q+:clear debug-logs)
    (let* ((current-log-level (config :logger :log-level :min-level-shown))
           (allowed-levels (member current-log-level *log-levels* :key #'car)))
      (dolist (log (reverse (log-list logger)))
        (destructuring-bind (time type formatted-message) log
          (when (find type allowed-levels :key #'car)
            (let ((html-message (htmlize-message type time formatted-message)))
              (signal! logger (new-log string) html-message))))))))

(defmethod note ((logger raptor-logger) type message &rest args)
  (let* ((formatted-message (apply #'format nil message args))
         (now (local-time:now))
         (current-log-level (config :logger :log-level :min-level-shown))
         (allowed-levels (member current-log-level *log-levels* :key #'car)))
    (push (list now type formatted-message) (log-list logger))
    (when (find type allowed-levels :key #'car)
      (let ((html-message (htmlize-message type now formatted-message)))
        (signal! logger (new-log string) html-message))))
  (values))

(defmethod logs ((logger raptor-logger) &key (severity :trace))
  (let* ((severities (mapcar #'car *log-levels*))
         (collected-severities (member severity severities)))
    (remove-if-not (rcurry #'member collected-severities) (log-list logger)
                   :key #'car)))

(defmethod clear ((logger raptor-logger))
  (signal! logger (clear-logs))
  (values))

(with-config-transaction ()
  (default-config :info :logger :log-level :min-level-shown)
  (loop for (type . color) in *log-levels*
        do (default-config color :logger :log-level :color type)))

(defparameter *htmlize-format-string* "
<p>
  <span style=\"color: #~{~2,'0X~};\">
    <b><code style=\"white-space: pre;\">[~6A]</code></b>
    ~2,'0D:~2,'0D:~2,'0D: ~A
  </span>
</p>")

(defun htmlize-message (type timestamp formatted-message)
  (let ((color (or (config :logger :log-level :color type) '(0 0 0)))
        (text (plump-dom:encode-entities formatted-message))
        (hour (local-time:timestamp-hour timestamp))
        (minute (local-time:timestamp-minute timestamp))
        (second (local-time:timestamp-second timestamp)))
    (format nil *htmlize-format-string* color type hour minute second text)))
