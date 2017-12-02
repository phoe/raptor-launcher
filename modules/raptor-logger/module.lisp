;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; module.lisp

(in-package :raptor-launcher/raptor-logger)
(in-readtable :qtools)

(define-raptor-module raptor-logger (logger)
  (:main-window qhboxlayout
                (log-list :accessor log-list :initform '()))
  (:selector "Logger")
  (:button clear-button "Clear")
  (:constructor))

(define-subwidget (raptor-logger debug-logs) (q+:make-qplaintextedit)
  (q+:add-widget layout debug-logs)
  (setf (q+:read-only debug-logs) t))

(define-signal (raptor-logger new-log) (string))

(define-slot (raptor-logger add-log) ((message string))
  (declare (connected raptor-logger (new-log string)))
  (q+:append-html debug-logs message))

(define-slot (raptor-logger clear-logs) ()
  (declare (connected clear-button (pressed)))
  (q+:clear debug-logs))

(defmethod note ((logger raptor-logger) type message &rest args)
  (let* ((formatted-message (apply #'format nil message args))
         (now (local-time:now))
         (html-message (htmlize-message type now formatted-message)))
    (push (list type formatted-message) (log-list logger))
    (signal! logger (new-log string) html-message))
  (values))

(defmethod logs ((logger raptor-logger))
  (log-list logger))

(defmethod clear ((logger raptor-logger))
  (signal! logger (clear-logs))
  (note logger :trace "Logs cleared.")
  (values))

(loop for (type . color) in *message-types*
      do (default-config color :raptor-config :message-type :color type))

(defparameter *htmlize-format-string* "
<p>
  <span style=\"color: #~{~2,'0X~};\">
    <b><code style=\"white-space: pre;\">[~6A]</code></b>
    ~2,'0D:~2,'0D:~2,'0D: ~A
  </span>
</p>")

(defun htmlize-message (type timestamp formatted-message)
  (let ((color (config :raptor-config :message-type :color type))
        (text (plump-dom:encode-entities formatted-message))
        (hour (local-time:timestamp-hour timestamp))
        (minute (local-time:timestamp-minute timestamp))
        (second (local-time:timestamp-second timestamp)))
    (format nil *htmlize-format-string* color type hour minute second text)))
