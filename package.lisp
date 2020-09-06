;;;; package.lisp

(defpackage #:gl-fourier-animation
  (:use #:cl #:cl-opengl #:bordeaux-fft)
  (:export
   #:main
   #:set-shape
   #:*violinschluessel-512*
   #:*achtel-512*
   #:*hessen-512*))
