;;;; gl-fourier-animation.asd

(asdf:defsystem #:gl-fourier-animation
  :description "Describe gl-fourier-animation here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :depends-on (:lispbuilder-sdl
               :cl-opengl ;;; #:safe-queue
               :cl-glut :cl-glu
               :png
               :cl-ppcre
               :bordeaux-fft )
  :serial t
  :components ((:file "package")
               (:file "window")
               (:file "utils")
               (:file "path-convert")
               (:file "fft-calculation")
               (:file "data")
               (:file "gl-fourier-animation")
               (:file "02-indexed-circles")))
