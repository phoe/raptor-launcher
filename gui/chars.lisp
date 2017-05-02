;;;; chars.lisp

(in-package :furcadia-launcher-gui)
(in-readtable :qtools)

(define-subwidget (launcher character-list) (q+:make-qtablewidget)
  (mapc (curry #'q+:insert-column character-list) '(0 1))
  (setf (q+:minimum-height character-list) 100
        (q+:column-width character-list 0) 200
        (q+:vertical-scroll-mode character-list)
        (q+:qabstractitemview.scroll-per-pixel)
        (q+:horizontal-scroll-mode character-list)
        (q+:qabstractitemview.scroll-per-pixel)
        (q+:horizontal-header-labels character-list)
        '("Name" "Last Login"))
  (let ((header (q+:horizontal-header character-list)))
    (setf (q+:stretch-last-section header) t)))

(defun add-character-to-list (widget character)
  ;; tableWidget->insertRow( tableWidget->rowCount() );
  (let ((row-count (q+:row-count widget))
        (name (assoc-value character :name))
        (name (assoc-value character :name))))
  )

(define-subwidget (launcher description-preview) (q+:make-qtextedit)
  (setf (q+:minimum-height description-preview) 100))

(define-subwidget (launcher chars-box) (q+:make-qsplitter)
  (q+:add-widget chars-box character-list)
  (q+:add-widget chars-box description-preview)
  (setf (q+:orientation chars-box) (q+:qt.vertical)
        (q+:children-collapsible chars-box) nil
        (q+:stretch-factor chars-box) (values 0 2)
        (q+:stretch-factor chars-box) (values 1 1)))
