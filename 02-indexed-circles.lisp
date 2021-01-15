;;; 
;;; 02-indexed-circles.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2020 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

(defconstant +float-size+ 4)
(defparameter *gl-scale* 5)
(defparameter *debug* t)
(defparameter *window* nil)

(defparameter *arrowhead* '(0.08 0.0075 0.1 0.0 0.08 -0.0075))

(defmacro with-program ((program) &body body)
  `(progn
     (gl:use-program ,program)
     ,@body
     (gl:use-program 0)))

(defmacro with-bound-buffer ((type buffer) &body body)
  `(progn
     (gl:bind-buffer ,type ,buffer)
     ,@body
     (gl:bind-buffer ,type 0)))

(defmacro with-bound-vertex-array ((vao) &body body)
  `(progn
     (gl:bind-vertex-array ,vao)
     ,@body
     (gl:bind-vertex-array 0)))

(defun get-offset (window mode)
  (case mode
    (1  (complex 0 0))
    (otherwise (shape-offset (shape window)))))

(defclass circle-window (glut:window)
  ((angle-incr :accessor angle-incr :initform 0.002)
   (grid-vbo :accessor grid-vbo)
   (grid-vao :accessor grid-vao)
   (grid-vbo-size :accessor grid-vbo-size)
   (circle-vbo :accessor circle-vbo)
   (arrowhead-vbo :accessor arrowhead-vbo)
   (arrowstem-vbo :accessor arrowstem-vbo)
   (shape-vbo :accessor shape-vbo)
   (coordinate-system-vbo :accessor coordinate-system-vbo)
   (offset-vbo :accessor offset-vbo)
   (angle-vbo :accessor angle-vbo)
   (shape-buffer :accessor shape-buffer)
   (circle-vao :accessor circle-vao)
   (arrowhead-vao :accessor arrowhead-vao)
   (arrowstem-vao :accessor arrowstem-vao)
   (shape-vao :accessor shape-vao)
   (circle-program :accessor circle-program)
   (arrow-program :accessor arrow-program)
   (shape-program :accessor shape-program)
   (last-pos :accessor last-pos :initform nil)
   (num :accessor num :initform 1024)
   (angle :accessor angle :initform 0)
   (mode :accessor mode :initform 3)
   (follow :accessor follow :initform nil)
   (curr-num :accessor curr-num :initform 512)
   (curr-path :accessor curr-path :initform nil)
   (curr-path-idx :accessor curr-path-idx :initform 0)
   (curr-path-length :accessor curr-path-length :initform 0)
   (zoom :accessor zoom :initform 1)
   (x-angle :accessor x-angle :initform 0)
   (y-angle :accessor y-angle :initform 0)
   (z-angle :accessor z-angle :initform 0)
   (shape :accessor shape :type (or null ) :initform nil)
   (mouse-start-x-angle :accessor mouse-start-x-angle :initform 0)
   (mouse-start-y-angle :accessor mouse-start-y-angle :initform 0)
   (mouse-start-z-angle :accessor mouse-start-z-angle :initform 0)
   (mouse-start-x :accessor mouse-start-x :type float :initform 0.0)
   (mouse-start-y :accessor mouse-start-y :type float :initform 0.0)) 
  (:default-initargs :width 400 :height 400 :pos-x 100 :pos-y 100
		     :mode '(:double :rgb :depth) :title "02-circles.lisp"))

(defun get-circle-verts (num radius)
  (apply #'vector ;;; circle around #(0 0)
         (loop for (pt1 pt2) on (loop for x below (1+ num)
                                      for theta = (mod (* 2 pi (/ x num)) num)
                                      collect `(,(float (* radius (cos theta)) 1.0)
                                                ,(float (* radius (sin theta)) 1.0)))
               while pt2
               append (append pt1 pt2))))

(defun get-grid-verts (range distance)
  "get 3D vertices of a coordinate grid plane extending from (0.0 0.0 0.0) to (scale scale 0.0)."
  (destructuring-bind (min-x min-y max-x max-y) range
    (apply #'vector
           (append
            (loop
              for x from min-x to max-x by distance
              append (list
                      (float x 1.0) (float min-y 1.0) -0.01
                      (float x 1.0) (float max-y 1.0) -0.01))
            (loop
              for y from min-y to max-y by distance
              append (list
                      (float min-x 1.0) (float y 1.0) 0.0
                      (float max-x 1.0) (float y 1.0) 0.0))
            (loop
              for z from -3 to 3 by 0.5
              for l = t then (not l)
              append (let ((len (if l 0.04 0.02)))
                       (list
                        0.0 (* len -1) (float z 1.0)
                        0.0 len (float z 1.0))))
            (loop
              for z from -3 to 3 by 0.5
              for l = t then (not l)
              append (let ((len (if l 0.04 0.02)))
                       (list
                        (* len -1) 0.0 (float z 1.0)
                        len 0.0 (float z 1.0))))
            (list 0.0 0.0 -4 0.0 0.0 4)))))

;;; (get-grid-verts 10)

(defmacro set-vbo-data ((type mode buffer) verts)
  `(let* ((arr (gl:alloc-gl-array :float (length ,verts))))
     (dotimes (i (length ,verts))
       (setf (gl:glaref arr i) (float (elt ,verts i) 1.0)))
     (with-bound-buffer (,type ,buffer)
       (gl:buffer-data ,type ,mode arr))
     (gl:free-gl-array arr)))

(defun get-idx (i mode shape)
  (with-slots (size fft-idx-sorted) shape
    (case mode
      (3 (elt fft-idx-sorted (mirror-list i size)))
      (2 (elt fft-idx-sorted i))
      (1 (mod (+ i *shift*) size))
      (otherwise (mirror-list2 i size)))))

(defun set-shape (window new-shape)
  (with-slots (shape num curr-num angle curr-path-length) window
    (setf shape new-shape)
    (setf curr-num (min curr-num (shape-size (shape window))))
    (setf angle 0)
    (setf curr-path-length 0)))

;;; (set-shape *window* *achtel-512*)
;;; (set-shape *window* *hessen-512*)
;;; (set-shape *window* *violinschluessel-512*)
;;; (set-shape *window* *sawtooth-512*)

;;; (curr-path-length *window*)

;;; (set-shape *window* *hessen-512*)

(defun set-offset-data (w)
  (with-slots (offset-vbo num) w
    (set-vbo-data (:array-buffer :dynamic-draw offset-vbo)
                  (loop for i below num append (list (- (* 0.2 (mod i 10)) 0.9)
                                                     (- (* 0.2 (mod (floor i 10) 10)) 0.9)
                                                     (+ 0.1 (random 0.7))
                                                     0.0)))))

(defmethod glut:display-window :before ((window circle-window))
  ;; An array buffer can be used to store verex position, colors,
  ;; normals, or other data. We need to allocate an GL array, copy the
  ;; data to the array, and tell OpenGL that the buffers data comes
  ;; from this GL array. Like most OpenGL state objects, we bind the
  ;; buffer before we can make changes to its state.
  (unless (gl::features-present-p (>= :glsl-version 3.3))
    (glut:destroy-current-window)
    (return-from glut:display-window nil))
  (with-slots (circle-vbo arrowhead-vbo arrowstem-vbo
               offset-vbo angle-vbo shape-vbo grid-vbo grid-vbo-size grid-vao
               circle-vao arrowhead-vao arrowstem-vao shape-vao
               circle-program arrow-program shape-program
               curr-path num)
      window

    (setf curr-path (make-array (list num) :initial-element (complex 0 1)))
    
    ;; Vertex array objects manage which vertex attributes are
    ;; associated with which data buffers. 

;;; setup shaders

    (let ((circle-vs (gl:create-shader :vertex-shader))
          (arrow-vs (gl:create-shader :vertex-shader))
          (shape-vs (gl:create-shader :vertex-shader))
          (fs (gl:create-shader :fragment-shader)))
      
      (gl:shader-source circle-vs (glsl-source "circle-vertex-shader.glsl"))
      (gl:compile-shader circle-vs)
      (gl:shader-source arrow-vs (glsl-source "arrow-vertex-shader.glsl"))
      (gl:compile-shader arrow-vs)
      (gl:shader-source shape-vs (glsl-source "shape-vertex-shader.glsl"))
      (gl:compile-shader shape-vs)
      (gl:shader-source fs (glsl-source "fragment-shader.glsl"))
      (gl:compile-shader fs)
      ;; If the shader doesn't compile, you can print errors with:
      (if *debug*
          (progn
            (print (gl:get-shader-info-log circle-vs))
            (print (gl:get-shader-info-log arrow-vs))
            (print (gl:get-shader-info-log shape-vs))
            (print (gl:get-shader-info-log fs))))

;;; setup programs

      (setf circle-program (gl:create-program))
      (gl:attach-shader circle-program circle-vs)
      (gl:attach-shader circle-program fs)
      (gl:link-program circle-program)

      (setf arrow-program (gl:create-program))
      (gl:attach-shader arrow-program arrow-vs)
      (gl:attach-shader arrow-program fs)
      (gl:link-program arrow-program)

      (setf shape-program (gl:create-program))
      (gl:attach-shader shape-program shape-vs)
      (gl:attach-shader shape-program fs)
      (gl:link-program shape-program)
      
;;; setup vbos and vaos
      
      (let ((buffers (gl:gen-buffers 8)))
        (setf grid-vbo (elt buffers 0))
        (setf circle-vbo (elt buffers 1))
        (setf arrowhead-vbo (elt buffers 2))
        (setf arrowstem-vbo (elt buffers 3))
        (setf shape-vbo (elt buffers 4))
        (setf offset-vbo (elt buffers 5))
        (setf angle-vbo (elt buffers 6)))
      (let ((verts (get-grid-verts '(-5 -5 5 5) (* *gl-scale* 0.1))))
        (set-vbo-data (:array-buffer :static-draw grid-vbo) verts)
        (setf  grid-vbo-size (/ (length verts) 6)))
      (set-vbo-data (:array-buffer :static-draw circle-vbo)
                    (get-circle-verts 64 (* *gl-scale* 0.1)))
      (set-vbo-data (:array-buffer :static-draw arrowhead-vbo)
                    (apply #'vector (mapcar (lambda (v) (* v *gl-scale*)) *arrowhead*)))
      (set-vbo-data (:array-buffer :static-draw arrowstem-vbo)
                    (apply #'vector (mapcar (lambda (v) (* v *gl-scale*)) '(0.0 0.002 0.08 0.002 0.08 -0.002 0.0 -0.002))))
      (set-vbo-data (:array-buffer :dynamic-draw shape-vbo)
                    (loop for i below (* 2 num) append (list 0.0 0.0 0.0)))
      (set-vbo-data (:array-buffer :dynamic-draw offset-vbo)
                    (loop for i below num append (list 0.0 0.0 0.0 0.0)))
      (set-vbo-data (:array-buffer :dynamic-draw angle-vbo)
                    (loop for i below num collect 0.0))

      
;;; circle setup indexed rendering:

;;; a vertex array object (vao) is a custom constellation of vbos,
;;; like an instance of some struct definition. The actual data is
;;; contained in the vbos. It gets instantiated with
;;; gl:gen-vertex-array. The following code instantiates an vao,
;;; defines the layout of the vbo data in the vao, enables the
;;; components and finally sets the vertex-attribute-divisor necessary
;;; for instanced rendering.
      
      (setf circle-vao (gl:gen-vertex-array))
      (with-bound-vertex-array (circle-vao)
        ;; To associate our CIRCLE-VBO data with this VAO, we bind it, specify
        ;; which vertex attribute we want to associate it with, and specify
        ;; where the data comes from.
        (with-bound-buffer (:array-buffer circle-vbo)
          ;; Using a null pointer as the data source indicates that we want
          ;; the vertex data to come from the currently bound array-buffer.
          (gl:enable-vertex-attrib-array 0)

        ;;; The vertex-attribute-pointer defines (for the currently
        ;;; bound vao) the number of values of one element and the
        ;;; step size to advance for each element in the vbo
        ;;; referenced by the first arg.
          (gl:vertex-attrib-pointer 0 2 :float nil (* 2 +float-size+) (cffi:null-pointer)))
        (with-bound-buffer (:array-buffer offset-vbo)
          (gl:enable-vertex-attrib-array 1)
          (gl:vertex-attrib-pointer 1 4 :float nil (* 4 +float-size+) (cffi:null-pointer)))
        ;;; The following vertex attribute-divisor states, that the
        ;;; elements (vertices) of the vbo at index 1 should only get
        ;;; advanced when all vertices upto index 1 have been
        ;;; processed. The vbo at index 0 contains the vertices of one
        ;;; circle, the vbo at index 1 contains the offsets and
        ;;; radiuses of the circles to draw. This means that all
        ;;; elements of the vbo at index 0 will get evaluated once for
        ;;; each element of the vbo at index 1 (aka "indexed
        ;;; rendering").
        (cl-opengl-bindings:vertex-attrib-divisor 1 1))

;;; arrowhead setup indexed rendering:
      
      (setf arrowhead-vao (gl:gen-vertex-array))

      (with-bound-vertex-array (arrowhead-vao)
        (with-bound-buffer (:array-buffer arrowhead-vbo)
          (gl:enable-vertex-attrib-array 0)
          (gl:vertex-attrib-pointer 0 2 :float nil (* 2 +float-size+) (cffi:null-pointer)))
        (with-bound-buffer (:array-buffer offset-vbo)
          (gl:enable-vertex-attrib-array 1)
          (gl:vertex-attrib-pointer 1 4 :float nil (* 4 +float-size+) (cffi:null-pointer)))
        (with-bound-buffer (:array-buffer angle-vbo)
          (gl:vertex-attrib-pointer 2 1 :float nil
                                    (* 1 +float-size+) (cffi:null-pointer)) ;;; idx corresponds to position in Vertex shader code
          (gl:enable-vertex-attrib-array 2)) ;;; idx corresponds to position in Vertex shader code
        (cl-opengl-bindings:vertex-attrib-divisor 1 1)
        (cl-opengl-bindings:vertex-attrib-divisor 2 1))
    
      (setf arrowstem-vao (gl:gen-vertex-array))
      (with-bound-vertex-array (arrowstem-vao)
        (with-bound-buffer (:array-buffer arrowstem-vbo)
          (gl:enable-vertex-attrib-array 0)
          (gl:vertex-attrib-pointer 0 2 :float nil (* 2 +float-size+) (cffi:null-pointer)))
        (with-bound-buffer (:array-buffer offset-vbo)
          (gl:enable-vertex-attrib-array 1)
          (gl:vertex-attrib-pointer 1 4 :float nil (* 4 +float-size+) (cffi:null-pointer)))

        (with-bound-buffer (:array-buffer angle-vbo)
          (gl:vertex-attrib-pointer 2 1 :float nil
                                    (* 1 +float-size+) (cffi:null-pointer)) ;;; idx corresponds to position in Vertex shader code
          (gl:enable-vertex-attrib-array 2)) ;;; idx corresponds to position in Vertex shader code

        (cl-opengl-bindings:vertex-attrib-divisor 1 1)
        (cl-opengl-bindings:vertex-attrib-divisor 2 1))

      (setf shape-vao (gl:gen-vertex-array))
      (with-bound-vertex-array (shape-vao)
        (with-bound-buffer (:array-buffer shape-vbo)
          (gl:enable-vertex-attrib-array 0)
          (gl:vertex-attrib-pointer 0 3 :float nil (* 3 +float-size+) (cffi:null-pointer))))

      (setf grid-vao (gl:gen-vertex-array))
      (with-bound-vertex-array (grid-vao)
        (with-bound-buffer (:array-buffer grid-vbo)
          (gl:enable-vertex-attrib-array 0)
          (gl:vertex-attrib-pointer 0 3 :float nil (* 3 +float-size+) (cffi:null-pointer))))

      
      (with-program (circle-program)
        (cl-opengl-bindings:uniform-4f (gl:get-uniform-location circle-program "Color") 1.0 0.3 0.0 0.5))
      (with-program (arrow-program)
        (cl-opengl-bindings:uniform-4f (gl:get-uniform-location arrow-program "Color") 0.6 0.6 0.6 1.0))
      (with-program (shape-program)
        (cl-opengl-bindings:uniform-4f (gl:get-uniform-location shape-program "Color") 1.0 1.0 0.0 1.0))
      (gl:delete-shader circle-vs)
      (gl:delete-shader arrow-vs)
      (gl:delete-shader shape-vs)
      (gl:delete-shader fs))))

(defmethod glut:idle ((w circle-window))
  (glut:post-redisplay))

(defun gl-init ()
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one)
  (gl:disable :depth-test)
  (gl:depth-func :lequal)

  (gl:clear-color 0 0 0 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:line-width 1))

(defun draw-shape (proj-mat program vao idx num)
  (with-program (program)
    (gl:line-width 2)
    (cl-opengl-bindings:uniform-4f (gl:get-uniform-location program "Color") 1.0 1.0 0.0 1.0)
    (gl:uniform-matrix 
     (gl:get-uniform-location program "projection") 4 (vector proj-mat) nil)
    (with-bound-vertex-array (vao)
      (gl:draw-arrays :lines (* 2 idx)  (* 2 num)))))

(defun draw-grid (proj-mat program vao num)
  (with-program (program)
    (gl:line-width 2)
    (cl-opengl-bindings:uniform-4f (gl:get-uniform-location program "Color") 0.5 0.5 1.0 0.4)
    (gl:uniform-matrix 
     (gl:get-uniform-location program "projection") 4 (vector proj-mat) nil)
    (with-bound-vertex-array (vao)
      (gl:draw-arrays :lines 0 (* 2 num)))))

(defun draw-circles (proj-mat program vao num)
  (with-program (program)
    (gl:line-width 2)
    (gl:uniform-matrix 
     (gl:get-uniform-location program "projection") 4 (vector proj-mat) nil)
    (with-bound-vertex-array (vao)
      (gl:draw-arrays-instanced :lines 0 129 num))))

(defun draw-arrows (proj-mat arrow-program arrowhead-vao arrowstem-vao num)
  (with-program (arrow-program)
    (gl:uniform-matrix
     (gl:get-uniform-location arrow-program "projection") 4 (vector proj-mat) nil)
    (gl:bind-vertex-array arrowhead-vao)
    (gl:draw-arrays-instanced :triangles 0 3 num)
    (with-bound-vertex-array (arrowstem-vao)
      (gl:draw-arrays-instanced :quads 0 4 num))))


(defparameter *print* t)

(setf *print* t)
(setf *print* nil)

(defmethod glut:display ((w circle-window))
  (update-swank)
  (if *draw?*
      (let (curr-pos)
        ;; (gl:clear :color-buffer-bit :depth-buffer-bit)
        (with-slots (angle
                     circle-program circle-vao
                     arrow-program arrowhead-vao arrowstem-vao
                     shape-program shape-vao grid-vao grid-vbo-size
                     offset-vbo angle-vbo shape-vbo
                     shape num curr-num zoom
                     curr-path curr-path-idx
                     curr-path-length
                     x-angle y-angle z-angle
                     last-pos angle-incr
                     mode follow) 
            w
          (continuable
            (gl-init)
            (gl:bind-buffer :array-buffer offset-vbo)
            (with-slots (freq-idx-transform-fn scale fft) shape
              (with-bound-buffer (:array-buffer offset-vbo)
                (gl:with-mapped-buffer (p1 :array-buffer :read-write)
                  (loop
                    for curr-offset = (complex 0.0 0.0) then next-offset
                    for i below curr-num
                    for n from 0
                    for x = (get-idx i mode shape)
                    for idx = (funcall freq-idx-transform-fn x)
                    for next-offset = (* *gl-scale* (exp (* +i+ idx angle))  (aref fft x))
                      then (+ next-offset (* *gl-scale* (exp (* +i+ idx angle)) (aref fft x)))
                    do
                       (let ((offs (* i 4)))
                         (setf (cffi:mem-aref p1 :float (+ offs 0))
                               (float (realpart curr-offset) 1.0))
                         (setf (cffi:mem-aref p1 :float (+ offs 1))
                               (float (imagpart curr-offset) 1.0))
                         ;; (setf (cffi:mem-aref p1 :float (+ offs 2)) (float (/ (max 0 angle) (* 2 pi)) 1.0))
                         0.01
                         (setf (cffi:mem-aref p1 :float (+ offs 3))
                               (float (* 10 (abs (aref fft x))) 1.0))
                         
                         ;; (setf (cffi:mem-aref p1 :float (+ offs 0))
                         ;;       (float (elt point 0) 1.0))
                         ;; (setf (cffi:mem-aref p1 :float (+ offs 1))
                         ;;       (float (elt point 1) 1.0))
                         ;; (setf (cffi:mem-aref p1 :float (+ offs 2))
                         ;;       (float 0.0 1.0))
                         ;; (setf (cffi:mem-aref p1 :float (+ offs 3))
                         ;;       (float (* 10 (elt point 2)) 1.0))
                         (if *print* (format t "~&~a: ~a ~a ~a ~a"
                                             i
                                             (cffi:mem-aref p1 :float (+ offs 0))
                                             (cffi:mem-aref p1 :float (+ offs 1))
                                             (cffi:mem-aref p1 :float (+ offs 2))
                                             (cffi:mem-aref p1 :float (+ offs 3))))
                         )
                    finally (progn
                              (setf curr-pos next-offset)
                              (setf (aref curr-path curr-path-idx) next-offset)
                              (setf curr-path-length (- num curr-path-idx))))))
              (with-bound-buffer (:array-buffer angle-vbo)
                (gl:with-mapped-buffer (p2 :array-buffer :read-write)
                  (loop
                    for i below curr-num
                    for x = (get-idx i mode shape)
                    for idx = (funcall freq-idx-transform-fn x)
                    do
                       (setf (cffi:mem-aref p2 :float i) (float (+ (phase (aref fft x)) (* angle idx)) 1.0))
                       ;;                 (setf (cffi:mem-aref p2 :float i) (float (elt point 3) 1.0))
                       (if *print* (format t " ~a~%"
                                           (cffi:mem-aref p2 :float i))))))
              (with-bound-buffer (:array-buffer shape-vbo)
                (gl:with-mapped-buffer (p3 :array-buffer :read-write)
                  (let ((offs (* curr-path-idx 6))
                        (next-offs (* (mod (1+ curr-path-idx) num) 6)))
                    (setf (cffi:mem-aref p3 :float (+ offs 0)) (float (realpart curr-pos) 1.0))
                    (setf (cffi:mem-aref p3 :float (+ offs 1)) (float (imagpart curr-pos) 1.0))
                    (setf (cffi:mem-aref p3 :float (+ offs 2)) (float (/ (max 0 angle) (* -2 pi)) 1.0))
                    (setf (cffi:mem-aref p3 :float (+ offs 3))
                          (cffi:mem-aref p3 :float (+ next-offs 0)))
                    (setf (cffi:mem-aref p3 :float (+ offs 4))
                          (cffi:mem-aref p3 :float (+ next-offs 1)))
                    (setf (cffi:mem-aref p3 :float (+ offs 5))
                          (if (= curr-path-idx (- num 1))
                              0.0
                              (float (cffi:mem-aref p3 :float (+ next-offs 2))))
                          )))))
            (setf last-pos curr-pos)
            (gl:matrix-mode :modelview)
            (gl:load-identity)
;;;            (glu:perspective 50 (/ (glut:width w) (glut:height w)) -10 1)
            (gl:viewport 0 0 (glut:width w) (glut:width w))
            (gl:translate 0 -0.5 0)
            (gl:scale (/ zoom 5) (/ zoom -5) (/ zoom 5))
            (gl:rotate x-angle 1 0 0)
            (gl:rotate y-angle 0 1 0)
            (gl:rotate z-angle 0 0 1)

            (if follow
                (gl:translate (* -1 (realpart curr-pos)) (* -1 (imagpart curr-pos)) 0))
;;;            (gl:translate 0 0 (* -1 (/ angle (* 2 pi))))
            (when circle-program
              (let ((proj-mat (gl:get-float :modelview-matrix)))
;;                (translate 0 0 (+ -0.0 (* angle 0.157)))
                (draw-circles (gl:get-float :modelview-matrix) circle-program circle-vao curr-num)
                (draw-arrows (gl:get-float :modelview-matrix) arrow-program arrowhead-vao arrowstem-vao curr-num)
                (translate 0 0 (+ -0.0 (* 3.14 angle 0.159)))
                (scale 1 1 3.14)
                (draw-shape (gl:get-float :modelview-matrix) shape-program shape-vao curr-path-idx (- num curr-path-idx))
                (if (>= curr-path-length (- num curr-path-idx 1))
                    (progn
                      (translate 0 0 1 )
                      (draw-shape (gl:get-float :modelview-matrix) shape-program shape-vao 0 curr-path-idx)))
                (draw-grid proj-mat shape-program grid-vao grid-vbo-size)))
            (setf angle (mod (+ angle angle-incr) (* 2 pi)))
            (setf curr-path-idx (- num (floor (* (/ angle (* 2 pi)) num)) 1))
;;            (setf *draw?* nil)
            ))))
  (glut:swap-buffers)
  (gl:finish))

(defmethod glut:reshape ((w circle-window) width height)
  (with-slots (circle-program arrow-program shape-program) w
    (gl:viewport 0 0 (min width height) (min width height))
    (gl:matrix-mode :modelview)
    ;; Ensure that projection matrix ratio always matches the window size ratio,
    ;; so the polygon will always look square.
    (gl:load-identity)
;;; (glu:perspective 50 (/ (glut:width window) (glut:height window)) -1 1)
;;; (gl:ortho 0 width 0 height -1 1)
;;; (gl:translate (* (- 1 gl-scale) gl-width) (* (- 1 gl-scale) gl-height) 0.0)
    (gl:scale 1 1 1)
    (gl:ortho -1 1 -1 1 -1 1)
    (when circle-program
      (with-program (circle-program)
        (gl:uniform-matrix 
         (gl:get-uniform-location circle-program "projection") 
         4 (vector (gl:get-float :modelview-matrix)) nil)))
    (when arrow-program
      (with-program (arrow-program)
        (gl:uniform-matrix 
         (gl:get-uniform-location arrow-program "projection") 
         4 (vector (gl:get-float :modelview-matrix)) nil)))
    (when shape-program
      (with-program (shape-program)
        (gl:uniform-matrix 
         (gl:get-uniform-location shape-program "projection") 
         4 (vector (gl:get-float :modelview-matrix)) nil)))
    (gl:matrix-mode :modelview)
    (gl:load-identity)
    (setf (glut:width w) width)
    (setf (glut:height w) height)))

(defmethod glut:mouse ((window circle-window) button state x y)
  (continuable
    ;; (when (eql button :wheel-down)
    ;;   (set-zoom (* *zoom* 0.99)))
    ;; (when (eql button :wheel-up)
    ;;   (set-zoom (* *zoom* 1.0101)))
;;;    (format t "~a ~a ~S~%" button state (equal (list :active-ctrl) (glut:get-modifiers)))
    (case button
      (:left-button
       (when (and (null (glut:get-modifiers)) (eql state :down))
         (setf (mouse-start-x window) x)
         (setf (mouse-start-y window) y)
         (setf (mouse-start-x-angle window) (x-angle window))
         (setf (mouse-start-y-angle window) (y-angle window))
         (setf (mouse-start-z-angle window) (z-angle window))
         ))
      (:wheel-down (if (eql state :down)
                       (cond
                         ((equal (list :active-ctrl) (glut:get-modifiers))
                          (setf (zoom window) (* (zoom window) 95/100)))
                         ((null (glut:get-modifiers))
                          (setf (angle-incr window) (* (angle-incr window) 95/100))))))
      (:wheel-up (if (eql state :down)
                       (cond
                         ((equal (list :active-ctrl) (glut:get-modifiers))
                          (setf (zoom window) (* (zoom window) 100/95)))
                         ((null (glut:get-modifiers))
                          (setf (angle-incr window) (* (angle-incr window) 100/95))))))
      (:right-button (if (eql state :down)
                         (cond
                           ((equal (list :active-ctrl) (glut:get-modifiers))
                            (setf (zoom window) 1.0))
                           ((null (glut:get-modifiers))
                            (setf (x-angle window) 0.0)
                            (setf (y-angle window) 0.0)
                            (setf (z-angle window) 0.0))))))
      ;; (when (and (eql button :right-button) (eql state :down))
    ;;   (push (make-boid-system
    ;;          `(,(float x 1.0) ,(float (* -1 y) 1.0) 0.0 0.0)
    ;;          *boids-per-click*
    ;;          window)
    ;;         (systems window))
    ;;   (format t "added boid system, ~s total~%" (length (systems window)))
    ;;   (format t " = ~:d boids~%" (reduce #'+ (systems window) :key 'boid-count)))
    ))

(defmethod glut:motion ((window circle-window) x y)
  (continuable
    (let* ((scale (/ 180 (min (glut:height window) (glut:width window))))
           (dx (* (- x (mouse-start-x window)) scale))
           (dy (* (- y (mouse-start-y window)) scale)))
      (setf (y-angle window) (+ (mouse-start-y-angle window) dx))
      (setf (x-angle window) (+ (mouse-start-x-angle window) dy))
;;; (format t "dx: ~,2f dy: ~,2f ~,2f ~,2f ~,6f~%" dx dy (glut:width window) (glut:height window) (/ (min (glut:height window) (glut:width window))));
      )))

(defmethod glut:keyboard ((w circle-window) key x y)
  (declare (ignore x y))
  (case key
    (#\Esc (glut:destroy-current-window))
    (#\f (setf (follow w) (not (follow w))))))

;; Cleanup.
;; Most of the objects we created have analogous deletion function.
(defmethod glut:close ((w circle-window))
  (when (slot-boundp w 'circle-program)
   (gl:delete-program (circle-program w))))

(defun 02-indexed-circles ()
  (let ((w (make-instance 'circle-window :width 1000 :height 800)))
    (setf *window* w)
    (set-shape w *achtel-512*)
    (unwind-protect
         (continuable
           (glut:display-window w))
      (when (not (glut::destroyed w))
         (setf (glut::destroyed w) t)
         (glut:destroy-window (glut:id w))))))

;;; (02-indexed-circles)
;;; (gl:named-buffer-storage)

(defparameter *points*
  `((0 0 0)
    (1 1 0)))

(defparameter *testvalues*
  `((0.0 0.0 1.1 0.0)
    (0.0 0.0 1.3 ,(/ pi 2))
    (0.0 0 2.0 1.5)))

(defparameter *draw?* nil)


;; (setf (curr-num *window*) 30)

(setf *draw?* t)

#|
(progn
  (setf *draw?* nil)
  (subseq (curr-path *window*) 0 (curr-path-length *window*)))


(curr-path-idx *window*)

(curr-path-length *window*)

(last-pos *window*)
(progn
(02-indexed-circles)
(setf (curr-num *window*) 30))

(setf (angle *window*) 0)
(setf (angle-incr *window*) 0.005)
(setf *print* t)
(setf *print* nil)

(set-shape *window* *violinschluessel-512*)

(set-shape *window* *achtel-512*))
(set-shape *window* *hessen-512*)
(set-shape *window* *sawtooth-512*)

(setf (mode *window*) 2)
(setf (mode *window*) 3)
(setf (mode *window*) 3)


(setf (zoom *window*) 1)
(progn
(setf (x-angle *window*) 45)
(setf (y-angle *window*) 45)
(setf (z-angle *window*) 45))

(progn
(setf (x-angle *window*) 0)
(setf (y-angle *window*) 90)
(setf (z-angle *window*) 0)
)

(progn
(setf (x-angle *window*) 0)
(setf (y-angle *window*) 0)
(setf (z-angle *window*) 0)
)

(setf (angle-incr *window*) 0.005)
(funcall (slot-value *curr-shape* 'freq-idx-transform-fn) 511)

(setf (curr-num *window*) 1)
(setf (curr-num *window*) 2)
(setf (curr-num *window*) 4)
(setf (curr-num *window*) 10)
(setf (curr-num *window*) 40)
(setf (curr-num *window*) 80)
(setf (curr-num *window*) 160)
(setf (curr-num *window*) 320)
(setf (curr-num *window*) 512)
(setf (follow *window*) t)

|#

