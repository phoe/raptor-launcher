;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAPTOR-LAUNCHER
;;;; © Michał "phoe" Herda 2017
;;;; rgb.lisp

(in-package :raptor-launcher/util)

;;; The following code was cannibalized from the simple-rgb project.
;;; https://github.com/wmannis/simple-rgb/blob/master/rgb.lisp

;;; Copyright (c) 2008 William S. Annis.  All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
;;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.

(deftype rgb () '(simple-array (unsigned-byte 8) (3)))

(deftype hsv () '(simple-array (single-float 0.0e0 1.0e0) (3)))

(declaim (inline rgb hsv rgb->hsv hsv->rgb rotate-hsv rotate-rgb))

(defun rgb (r g b)
  (declare (optimize speed))
  (make-array '(3) :element-type '(unsigned-byte 8)
                   :initial-contents (list r g b)))

(defun hsv (h s v)
  (declare (optimize speed))
  (make-array '(3) :element-type '(single-float 0.0e0 1.0e0)
                   :initial-contents (list h s v)))

(declaim (ftype (function (rgb) hsv) rgb->hsv))

(defun rgb->hsv (a)
  (declare (optimize speed))
  (declare (type rgb a))
  (let* ((r (* (aref a 0) (coerce 1/255 'single-float)))
         (g (* (aref a 1) (coerce 1/255 'single-float)))
         (b (* (aref a 2) (coerce 1/255 'single-float)))
         (max (max r g b))
         (min (min r g b))
         (v max))
    (if (= max min)
        (make-array '(3) :element-type '(single-float 0.0e0 1.0e0)
                         :initial-contents (list 0.0e0 0.0e0 v))
        (let ((tmp (- max min))
              (s (/ (- max min) max))
              (h 0.0e0))
          (cond ((= r max)
                 (setf h (- (/ (- max b) tmp)
                            (/ (- max g) tmp))))
                ((= g max)
                 (setf h (+ 2.0e0 (- (/ (- max r) tmp)
                                     (/ (- max b) tmp)))))
                (t
                 (setf h (+ 4.0e0 (- (/ (- max g) tmp)
                                     (/ (- max r) tmp))))))
          (let ((foo (* h (coerce 1/6 'single-float))))
            (setf h (mod foo 1)))
          (hsv h s v)))))

(declaim (ftype (function (hsv) rgb) hsv->rgb))

(defun hsv->rgb (a)
  (declare (optimize speed))
  (declare (type hsv a))
  (let ((h (aref a 0))
        (s (aref a 1))
        (v (aref a 2)))
    (labels ((unfloat (c)
               (declare (type (single-float 0.0e0 1.0e0) c))
               (coerce (round (* c 255)) 'integer))
             (rgb-from-floats (r g b)
               (declare (type (single-float 0.0e0 1.0e0) r g b))
               (make-array 3 :element-type '(unsigned-byte 8)
                             :initial-contents (list (unfloat r)
                                                     (unfloat g)
                                                     (unfloat b)))))
      (the rgb (if (= s 0.0)
                   (rgb-from-floats v v v)
                   (multiple-value-bind (i f) (truncate (* h 6.0))
                     (declare (type (integer 0 6) i))
                     (let* ((p (* v (- 1.0e0 s)))
                            (q (* v (- 1.0e0 (* s f))))
                            (tv (* v (- 1.0e0 (* s (- 1.0 f))))))
                       (cond ((= i 1) (rgb-from-floats q v p))
                             ((= i 2) (rgb-from-floats p v tv))
                             ((= i 3) (rgb-from-floats p q v))
                             ((= i 4) (rgb-from-floats tv p v))
                             ((= i 5) (rgb-from-floats v p q))
                             (t (rgb-from-floats v tv p))))))))))

(defun rotate-hsv (a rotation)
  (declare (optimize speed))
  (declare (type hsv a)
           (type (integer 0 359) rotation))
  (let ((h (aref a 0))
        (s (aref a 1))
        (v (aref a 2))
        (scaled-rotation (* rotation (coerce 1/360 'single-float))))
    (the hsv (hsv (mod (+ h scaled-rotation) 1.0) s v))))

(defun rotate-rgb (a rotation)
  (declare (optimize speed))
  (declare (type rgb a)
           (type (integer 0 359) rotation))
  (let ((hsv (rgb->hsv a)))
    (declare (type hsv hsv))
    (let ((rotated-hsv (rotate-hsv hsv rotation)))
      (declare (type hsv rotated-hsv))
      (let ((rgb (hsv->rgb rotated-hsv)))
        (declare (type rgb rgb))
        rgb))))
