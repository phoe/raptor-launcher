;;;; chars.lisp

(in-package :furcadia-launcher-gui)
(in-readtable :qtools)

(define-subwidget (launcher character-list) (q+:make-qtablewidget 0 2)
  (setf (q+:minimum-height character-list) 100
        (q+:column-width character-list 0) 170
        (q+:vertical-scroll-mode character-list)
        (q+:qabstractitemview.scroll-per-pixel)
        (q+:horizontal-scroll-mode character-list)
        (q+:qabstractitemview.scroll-per-pixel)
        (q+:horizontal-header-labels character-list)
        '("Name" "Last Login")
        (q+:selection-behavior character-list)
        (q+:qabstractitemview.select-rows))
  (let ((h-header (q+:horizontal-header character-list))
        (v-header (q+:vertical-header character-list)))
    (setf (q+:stretch-last-section h-header) t
          (q+:resize-mode v-header) (q+:qheaderview.fixed)
          (q+:default-section-size v-header) 20
          (q+:sort-indicator h-header 0) (q+:qt.ascending-order))
    (q+:hide v-header))
  (mapc (curry #'add-character-to-list character-list)
        (furcadia-launcher::state-last-logins))
  (q+:clear-selection character-list)
  (setf (q+:sorting-enabled character-list) t))

(defun add-character-to-list (widget character)
  (let* ((count (q+:row-count widget))
         (name (assoc-value character :name))
         (timestamp (assoc-value character :login-date))
         (datestring (furcadia-launcher::unix-time-to-datestring timestamp)))
    (q+:insert-row widget count)
    (put-table-text widget name count 0)
    (put-table-text widget datestring count 1)))

;; TODO turn into SETF TABLE-TEXT
(defun put-table-text (widget text row column)
  (when (null-qobject-p (q+:item widget row column))
    (setf (q+:item widget row column) (q+:make-qtablewidgetitem)))
  (let ((item (q+:item widget row column)))
    (setf (q+:text item) text)))

(defun selected-character-sname (widget)
  (let* ((list (q+:selected-indexes widget))
         (index (find 0 list :key #'q+:column))
         (row (q+:row index))
         (column (q+:column index))
         (item (q+:item widget row column))
         (name (q+:text item))
         (last-logins (furcadia-launcher::state-last-logins))
         (alist (find name last-logins :key (rcurry #'assoc-value :name)
                                       :test #'string=)))
    (assoc-value alist :shortname)))

(define-subwidget (launcher description-preview) (q+:make-qtextbrowser)
  (setf (q+:minimum-height description-preview) 100
        (q+:text-interaction-flags description-preview)
        (q+:qt.text-selectable-by-mouse)
        (q+:text-interaction-flags description-preview)
        (q+:qt.text-browser-interaction)
        (q+:open-external-links description-preview) t))

(define-slot (launcher char-selection-change) ()
  (declare (connected character-list (item-selection-changed)))
  (let* ((sname (selected-character-sname character-list))
         (characters (getf furcadia-launcher::*config* :characters))
         (character (cdr (find sname characters :key #'car :test #'string=)))
         (description (assoc-value character :desc)))
    (setf (q+:text description-preview) description)))

(define-subwidget (launcher chars-box) (q+:make-qsplitter)
  (q+:add-widget chars-box character-list)
  (q+:add-widget chars-box description-preview)
  (setf (q+:orientation chars-box) (q+:qt.vertical)
        (q+:children-collapsible chars-box) nil
        (q+:stretch-factor chars-box) (values 0 2)
        (q+:stretch-factor chars-box) (values 1 1)))
