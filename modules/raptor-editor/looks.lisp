;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; looks.lisp

(in-package :raptor-launcher/raptor-editor)
(in-readtable :qtools)

(define-widget looks (qwidget)
  ((module :accessor module
           :initarg :module)
   (name :accessor name
         :initarg :name
         :initform "Looks")
   (palette :accessor palette
            :initarg :palette)
   (nbuttons :accessor nbuttons
             :initform 0)
   (buttons-per-row :accessor buttons-per-row
                    :initform 7)))

(define-subwidget (looks layout) (q+:make-qvboxlayout)
  (setf (q+:layout looks) layout))

(define-subwidget (looks buttons) (q+:make-qgridlayout)
  (q+:add-layout layout buttons 1))

(define-subwidget (looks palettes) (q+:make-qstackedlayout)
  (q+:add-layout layout palettes 9001))

(define-qt-constructor (looks)
  (with-slots (tabs) module
    (q+:add-tab tabs looks name))
  (let*
      ((color-types *color-types*)
       (names (mapcar (compose #'string-capitalize #'symbol-name) color-types))
       (fn (lambda (x y) (make-instance 'color-palette :stacked-widget palettes
                                                       :name x :color-type y)))
       (instances (mapcar fn names color-types))
       (selectors (mapcar #'selector instances)))
    (loop for palette in instances
          for selector in selectors
          for row = (floor nbuttons buttons-per-row)
          for column = (mod nbuttons buttons-per-row)
          do (q+:add-widget palettes palette)
             (setf (q+:size-policy selector) (values (q+:qsizepolicy.expanding)
                                                     (q+:qsizepolicy.fixed)))
             (q+:add-widget buttons selector row column)
             (incf nbuttons))))

#|
(progn
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defun %define-looks-button (keyword i)
      (multiple-value-bind (mod rem) (floor i 7)
        (let ((name (symbolicate keyword "-BUTTON"))
              (text (string-capitalize (symbol-name keyword))))
          `(define-subwidget (looks ,name) (make-text-qtoolbutton ,text)
             (q+:add-widget layout ,name ,mod ,rem)
             (setf (q+:size-policy ,name)
                   (values (q+:qsizepolicy.expanding)
                           (q+:qsizepolicy.expanding)))))))
    (defparameter *looks-buttons*
      (list* :images :digo *color-types*)))
  (defmacro define-looks-buttons (list)
    `(progn ,@(loop for keyword in (eval list)
                    for i from 0
                    collect (%define-looks-button keyword i))))
  (define-looks-buttons *looks-buttons*))

;; (define-subwidget (looks gradient) (q+:make-qwidget)
;;   (q+:add-widget layout gradient 2 0 1 1)
;;   (setf (q+:row-stretch layout 2) 9001
;;         (q+:style-sheet gradient) "background-color: white;"))

;; (define-subwidget (looks picker) (q+:make-qwidget)
;;   (q+:add-widget layout picker 2 1 1 6)
;;   (setf (q+:style-sheet picker) "background-color: gray;"))
|#
