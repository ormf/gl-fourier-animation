;;; 
;;; examples.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2021 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
;;;
;;; Revision history: See git repository.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Gnu Public License, version 2 or
;;; later. See https://www.gnu.org/licenses/gpl-2.0.html for the text
;;; of this agreement.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; **********************************************************************

(in-package :gl-fourier-animation)

(progn
  (setf (continuous-p *window*) t)
  (gl-enqueue (gl-set-fft-idxs '(1 511) *window*) *window*)
  (gl-enqueue (gl-set-mode 2 *window*) *window*)
  (gl-enqueue (gl-set-shape *cos-pulse-512* *window*) *window*))

(progn
  (setf (continuous-p *window*) t)
  (gl-enqueue (gl-set-fft-idxs '(1 511) *window*) *window*)
  (gl-enqueue (gl-set-mode 1 *window*) *window*)
  (gl-enqueue (gl-set-shape *cos-pulse-512* *window*) *window*))

(progn
  (setf (continuous-p *window*) t)
  (gl-enqueue (gl-set-fft-idxs '(1 511) *window*) *window*)
  (gl-enqueue (gl-set-mode 1 *window*) *window*)
  (gl-enqueue (gl-set-shape *sin-pulse-512* *window*) *window*))

(progn
  (gl-enqueue (gl-set-fft-idxs '(1 7) *window*) *window*)
  (gl-enqueue (gl-set-mode 3 *window*) *window*)
  (gl-enqueue (gl-set-shape *cos-pulse-8* *window*) *window*))

(progn
  (gl-enqueue (gl-set-fft-idxs '(1 7) *window*) *window*)
  (gl-enqueue (gl-set-mode 3 *window*) *window*)
  (gl-enqueue (gl-set-shape *sin-pulse-8* *window*) *window*)
  (setf (angle *window*) 0)
  (setf (draw-once-p *window*) t))

(gl-enqueue (gl-set-fft-idxs '(0 4) *window*) *window*)
(gl-enqueue (gl-set-fft-idxs '(1 7) *window*) *window*)

(progn
  (setf (continuous-p *window*) nil)
)

(progn
)
(gl-enqueue (gl-set-fft-idxs '(1 7) *window*) *window*)
(gl-enqueue (gl-set-fft-idxs '(2 6) *window*) *window*)
(gl-enqueue (gl-set-fft-idxs '(3 5) *window*) *window*)
(gl-enqueue (gl-set-fft-idxs '(0 4) *window*) *window*)

(progn
  (gl-enqueue (gl-set-fft-idxs '(1 511) *window*) *window*)
  (gl-enqueue (gl-set-shape *cos-pulse-512* *window*) *window*))

(progn
  (gl-enqueue (gl-set-fft-idxs '(1) *window*) *window*)
  (gl-enqueue (gl-set-shape *cos-pulse-512-norm* *window*) *window*))

(gl-enqueue (gl-set-fft-idxs '(511) *window*) *window*)
(gl-enqueue (gl-set-fft-idxs '(1) *window*) *window*)
(gl-enqueue (gl-set-fft-idxs '(2) *window*) *window*)
(gl-enqueue (gl-set-fft-idxs '(3) *window*) *window*)
(gl-enqueue (gl-set-fft-idxs '(4) *window*) *window*)
(gl-enqueue (gl-set-fft-idxs '(5) *window*) *window*)

(progn
  (gl-enqueue (gl-set-shape *cos-pulse-8* *window*) *window*)
  (gl-enqueue (gl-set-fft-idxs '(1) *window*) *window*))
