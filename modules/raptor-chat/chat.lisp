;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; chat.lisp

(in-package :raptor-launcher/raptor-chat)
(in-readtable :qtools)

(define-widget chat-window (qwidget) ())

(define-subwidget (chat-window layout) (q+:make-qgridlayout)
  (setf (q+:layout chat-window) layout))

;;; Upper row

(define-subwidget (chat-window description-button-left)
    (make-text-qtoolbutton "Description")
  (let ((font (q+:make-qfont)))
    (setf (q+:point-size font) 8
          (q+:font description-button-left) font))
  (q+:add-widget layout description-button-left 0 0))

(define-subwidget (chat-window buttons-layout) (q+:make-qhboxlayout)
  (q+:add-layout layout buttons-layout 0 1)
  (flet ((add (&rest strings)
           (loop for string in strings
                 for button = (make-chat-button string)
                 do (q+:add-widget buttons-layout button))))
    (add "Mark Read" "Logs" "Justify")
    (q+:add-stretch buttons-layout 9001)
    (add "Timestamps off" "Dictionary" "Thesaurus")))

(define-subwidget (chat-window spellchecker-button)
    (make-chat-button "Spellchecker")
  (q+:add-widget buttons-layout spellchecker-button))

(defun make-chat-button (string)
  (let ((button (make-text-qtoolbutton string))
        (font (q+:make-qfont)))
    (setf (q+:point-size font) 8
          (q+:font button) font)
    button))

(define-subwidget (chat-window description-button-right)
    (make-text-qtoolbutton "Description")
  (let ((font (q+:make-qfont)))
    (setf (q+:point-size font) 8
          (q+:font description-button-right) font))
  (q+:add-widget layout description-button-right 0 2 2))

;;; Bottom row

(define-subwidget (chat-window image-left) (q+:make-qlabel)
  (q+:add-widget layout image-left 1 0)
  (setf (q+:pixmap image-left)
        (q+:make-qpixmap
         "/home/phoe/Projects/Qt Designer/raptor-chat/undies.png")))

(define-subwidget (chat-window splitter)
    (q+:make-qsplitter (q+:qt.horizontal))
  (q+:add-widget layout splitter 1 1))

(define-subwidget (chat-window image-right) (q+:make-qlabel)
  (q+:add-widget layout image-right 1 2)
  (setf (q+:pixmap image-right)
        (q+:make-qpixmap
         "/home/phoe/Projects/Qt Designer/raptor-chat/sha.png")))

;;; IC/OOC

(define-subwidget (chat-window ic) (q+:make-qsplitter (q+:qt.vertical))
  (q+:add-widget splitter ic)
  (setf (q+:minimum-width ic) 100
        (q+:children-collapsible ic) nil
        (q+:stretch-factor splitter 0) 2))

(define-subwidget (chat-window ooc) (q+:make-qsplitter (q+:qt.vertical))
  (q+:add-widget splitter ooc)
  (setf (q+:minimum-width ooc) 100
        (q+:children-collapsible ooc) nil
        (q+:stretch-factor splitter 1) 1))

(define-subwidget (chat-window ic-output)
    (make-placeholder-text-edit "No IC chat yet." 1.25)
  (q+:add-widget ic ic-output)
  (setf (q+:read-only ic-output) t
        (q+:minimum-height ic-output) 100
        (q+:stretch-factor ic 0) 3)
  (setf (q+:html ic-output)
        (read-file-into-string
         "/home/phoe/Projects/Qt Designer/raptor-chat/ic.txt")))

(define-subwidget (chat-window ic-input)
    (make-placeholder-text-edit "Type your IC here!")
  (q+:add-widget ic ic-input)
  (setf (q+:minimum-height ic-input) 100
        (q+:stretch-factor ic 1) 1
        (q+:text ic-input)
        "Hello world! This, as yyou can see, jjkhdssdfs ain't no joke."))

(define-subwidget (chat-window ooc-output)
    (make-placeholder-text-edit "[No OOC chat yet.]" 1.25)
  (q+:add-widget ooc ooc-output)
  (setf (q+:read-only ooc-output) t
        (q+:minimum-height ooc-output) 100
        (q+:stretch-factor ooc 0) 3)
  (setf (q+:html ooc-output)
        (read-file-into-string
         "/home/phoe/Projects/Qt Designer/raptor-chat/ooc.txt")))

(define-subwidget (chat-window ooc-input)
    (make-placeholder-text-edit "[Type your OOC here!]")
  (q+:add-widget ooc ooc-input)
  (setf (q+:minimum-height ooc-input) 100
        (q+:stretch-factor ooc 1) 1))

;;; PLACEHOLDER-TEXT-EDIT

(define-widget placeholder-text-edit (qtextedit)
  ((placeholder :accessor placeholder :initarg :placeholder)
   (font :accessor font :initarg :font))
  (:default-initargs :placeholder "" :font (q+:make-qfont)))

(defmethod initialize-instance :after
    ((object placeholder-text-edit) &key font-size)
  (with-accessors ((font font)) object
    (setf (q+:italic font) t)
    (when font-size
      (setf (q+:point-size-f font)
            (* font-size (q+:point-size-f font))))))

(define-override (placeholder-text-edit paint-event) (ev)
  (when (string= "" (q+:to-plain-text placeholder-text-edit))
    (let ((viewport (q+:viewport placeholder-text-edit)))
      (with-finalizing ((painter (q+:make-qpainter viewport)))
        (let* ((color (q+:color (q+:brush (q+:pen painter))))
               (old-font (q+:font painter)))
          (setf (q+:alpha color) 80
                (q+:font painter) font)
          (q+:draw-text painter (q+:rect placeholder-text-edit)
                        (logior (q+:qt.text-word-wrap)
                                (q+:qt.align-center))
                        (placeholder placeholder-text-edit))
          (setf (q+:alpha color) 255
                (q+:font painter) old-font)))))
  (call-next-qmethod))

(define-widget placechecked-text-edit
      (qtextedit placeholder-text-edit spellchecked-text-edit) ())

(defun make-placeholder-text-edit (placeholder &optional font-size)
  (make-instance 'placechecked-text-edit :placeholder placeholder
                                         :font-size font-size))

(define-slot (chat-window spellchecker-button-clicked) ()
  (declare (connected spellchecker-button (clicked)))
  (let ((position (with-finalizing ((cursor (q+:text-cursor ic-input)))
                    (q+:position cursor))))
    (spellcheck ic-input position)))
