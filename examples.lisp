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

;;; start gl window
(02-indexed-circles)

;;; reset view angles

(progn
  (setf (x-angle *window*) 0)
  (setf (y-angle *window*) 0)
  (setf (z-angle *window*) 0))

;;; set speed of animation

(setf *angle-incr* 0.2)

;;; load other shapes

(gl-enqueue (gl-set-shape *sawtooth-512* *window*) *window*)
(gl-enqueue (gl-set-shape *violinschluessel-512* *window*) *window*)
(gl-enqueue (gl-set-shape *hessen-512* *window*) *window*)

;;; a real valued cosine wave

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

(gl-enqueue (gl-set-shape *violinschluessel-512* *window*) *window*)
(gl-enqueue (gl-set-shape *achtel-512* *window*) *window*)

(gl-enqueue (gl-set-shape *sawtooth-512* *window*) *window*)

(gl-enqueue (gl-set-shape *hessen-512* *window*) *window*)
(gl-enqueue (gl-set-shape *sawtooth-8* *window*) *window*)
(setf (continuous-p *window*) t)

(setf (continuous-p *window*) nil)
(gl-enqueue (gl-set-shape *sawtooth-512* *window*) *window*)
(progn
  (setf (continuous-p *window*) t)
  (gl-enqueue (gl-set-fft-idxs '(1 2 510 511) *window*) *window*)
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

(gl-enqueue (gl-set-fft-idxs '(1 511) *window*) *window*)
(gl-enqueue (gl-set-fft-idxs '(1 2 510 511) *window*) *window*)
(gl-enqueue (gl-set-fft-idxs '(1 2 3 509 510 511) *window*) *window*)
(gl-enqueue (gl-set-fft-idxs '(1 2 3 4 508 509 510 511) *window*) *window*)
(gl-enqueue (gl-set-fft-idxs '(1 2 3 4 5 507 508 509 510 511) *window*) *window*)
(gl-enqueue (gl-set-fft-idxs '(1 2 3 4 5 507 508 509 510 511) *window*) *window*)
(gl-enqueue (gl-set-fft-idxs '(2 510) *window*) *window*)
(gl-enqueue (gl-set-fft-idxs '(3 509) *window*) *window*)


(gl-enqueue (gl-set-fft-idxs '(0 4) *window*) *window*)
(gl-enqueue (gl-set-fft-idxs '(1 7) *window*) *window*)

(progn
  (setf (continuous-p *window*) nil))

(gl-enqueue (gl-set-fft-idxs '(1 7) *window*) *window*)
(gl-enqueue (gl-set-fft-idxs '(2 6) *window*) *window*)
(gl-enqueue (gl-set-fft-idxs '(3 5) *window*) *window*)
(gl-enqueue (gl-set-fft-idxs '(0 4) *window*) *window*)

(progn
  (gl-enqueue (gl-set-fft-idxs '(1 511) *window*) *window*)
  (gl-enqueue (gl-set-shape *cos-pulse-512* *window*) *window*))

(setf (continuous-p *window*) t)

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

(gl-enqueue (gl-set-shape *sin-pulse-512* *window*) *window*)

(progn
  (setf (x-angle *window*) 0)
  (setf (y-angle *window*) 0)
  (setf (z-angle *window*) 0))

(setf (x-angle *window*) 300)

(progn
  (setf (x-angle *window*) 270)
  (setf (y-angle *window*) 90)
  (setf (z-angle *window*) 0))

(progn
  (setf (x-angle *window*) 180)
  (setf (y-angle *window*) 90) ;;; hier
  (setf (z-angle *window*) 0))
