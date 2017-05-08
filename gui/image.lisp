;;;; image.lisp

(in-package :furcadia-launcher-gui)
(in-readtable :qtools)

(defvar *character-image-dir* "~/.furcadia-launcher/images/"
  "The directory where character images and stored.")

(defvar *character-image-empty* "No Character Image
\(click here to add)")

(define-subwidget (launcher image) (q+:make-qpushbutton *character-image-empty*)
  (setf (q+:minimum-width image) 150
        (q+:maximum-width image) 150
        (q+:flat image) t
        (q+:size-policy image) (values (q+:qsizepolicy.expanding)
                                       (q+:qsizepolicy.expanding))))

(defun image-select-file (widget)
  (let ((dialog (q+:make-qfiledialog widget "Select image" "."
                                     "PNG/JPG Image Files (*.png *.jpg)")))
    (setf (q+:name-filter dialog) "PNG/JPG Image Files (*.png *.jpg)")
    (print (q+:exec dialog))
    (let ((return-value (first (q+:selected-files dialog))))
      (finalize dialog)
      return-value))
  ;; (let ((path (q+:qfiledialog-get-open-file-name
  ;;              (null-qobject "QWidget")
  ;;              "Select image"
  ;;              "." "PNG/JPG Image Files (*.png *.jpg)")))
  ;;   (if (string= path "") nil path))
  )

(define-slot (launcher image-clicked) ()
  (declare (connected image (clicked)))
  (let ((snames (selected-characters character-list)))
    (when (= 1 (length snames))
      (when-let ((from-path (image-select-file launcher)))
        (let* ((sname (first snames))
               (from-type (pathname-type from-path))
               (filename (cat sname "." (string-downcase from-type)))
               (to-pathname (merge-pathnames filename *character-image-dir*))
               (to-path (uiop:native-namestring to-pathname)))
          (ensure-directories-exist *character-image-dir*)
          (delete-character-image sname)
          (copy-file from-path to-path :finish-output t)
          (note :info "Selected image copied to ~A." filename)
          (signal! launcher (update-image)))))))

(define-signal (launcher update-image) ())

(define-slot (launcher update-image) ()
  (declare (connected launcher (update-image)))
  (declare (connected character-list (item-selection-changed)))
  (let* ((snames (selected-characters character-list)))
    (when (= 1 (length snames))
      (let ((sname (first snames)))
        (if-let ((path (selected-character-image-path sname)))
          (let* ((old-icon (q+:icon image))
                 (abs-path (princ-to-string (truename path)))
                 (pixmap (q+:make-qpixmap abs-path))
                 (new-icon (q+:make-qicon pixmap)))
            (setf (q+:text image) ""
                  (q+:icon image) new-icon
                  (q+:icon-size image) (q+:size (q+:rect pixmap)))
            (when old-icon
              (finalize old-icon)))
          (clear-image image))))))

(defun clear-image (image)
  (when (string= (q+:text image) "")
    (setf (q+:text image) *character-image-empty*
          (q+:icon image) (q+:make-qicon))))

(defun selected-character-image-path (sname)
  (let* ((path-base (cat *character-image-dir* sname))
         (png-path (cat path-base ".png"))
         (jpg-path (cat path-base ".jpg")))
    (cond ((probe-file png-path) png-path)
          ((probe-file jpg-path) jpg-path)
          (t nil))))

(defun delete-character-image (sname)
  (let* ((path-base (cat *character-image-dir* sname))
         (png-path (cat path-base ".png"))
         (jpg-path (cat path-base ".jpg")))
    (when (probe-file png-path) (delete-file png-path))
    (when (probe-file jpg-path) (delete-file jpg-path))))
