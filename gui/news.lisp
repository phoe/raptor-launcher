;;;; news.lisp

(in-package :furcadia-launcher-gui)
(in-readtable :qtools)

(defun generate-news-text (heading datestring news-type contents read-more-link)
  (concatenate 'string
               (format nil "<h4>~A</h4>~%<h5>~A - ~A</h5><p>~A</p>"
                       heading datestring news-type contents)
               (if (string= "" read-more-link) ""
                   (format nil "<a href=\"~A\">Read more.</a>"
                           read-more-link))))

(defun make-news-widget (news-line)
  (destructuring-bind (date filename datestring news-type heading contents
                       read-more-link image-link) news-line
    (declare (ignore date image-link))
    (let* ((widget (q+:make-qwidget))
           (layout (q+:make-qhboxlayout))
           (pathname (furcadia-launcher::news-image-filename-pathname filename))
           (pixmap (q+:make-qpixmap (princ-to-string (truename pathname))))
           (image (q+:make-qlabel))
           (text (q+:make-qlabel)))
      (setf (q+:layout widget) layout
            (q+:pixmap image) pixmap
            (q+:minimum-size image) (values 84 100)
            (q+:word-wrap text) t
            (q+:text-interaction-flags text)
            (q+:qt.text-browser-interaction)
            (q+:open-external-links text) t
            (q+:text text) (generate-news-text heading datestring news-type
                                               contents read-more-link))
      (q+:add-widget layout image)
      (q+:add-widget layout text)
      widget)))

(define-subwidget (launcher news-contents) (q+:make-qvboxlayout)
  (setf (q+:contents-margins news-contents) (values 0 0 0 0))
  (bt:make-thread (lambda ()
                    (furcadia-launcher::get-all-newsf)
                    (signal! launcher (enable-news)))
                  :name "Raptor Launcher news downloader"))

(define-subwidget (launcher news-contents-widget) (q+:make-qwidget)
  (setf (q+:layout news-contents-widget) news-contents))

(define-subwidget (launcher news-scroll) (q+:make-qscrollarea)
  (setf (q+:widget news-scroll) news-contents-widget
        (q+:widget-resizable news-scroll) t
        (q+:frame-shape news-scroll) (q+:qframe.no-frame)))

(define-subwidget (launcher news-layout) (q+:make-qhboxlayout)
  (setf (q+:contents-margins news-layout) (values 0 0 0 0))
  (q+:add-widget news-layout news-scroll))

(define-subwidget (launcher news-box) (q+:make-qwidget)
  (setf (q+:layout news-box) news-layout))

(define-signal (launcher enable-news) ())
