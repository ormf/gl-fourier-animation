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

(defstruct movement
  (numsteps 60)
  (curr-step 0)
  (target-x-angle nil)
  (target-y-angle nil)
  (target-z-angle nil)
  (target-zoom nil)
  (target-translation nil)
  (active nil))

(defun iinterpl (curr target step numsteps)
  "incremental interpolation to target"
  (+ curr (* (- target curr) (/ (- numsteps step)))))

(defclass circle-window (glut:window)
  ((gl-queue :accessor gl-queue :initform nil) ;;; list of closures to evaluate in the gl-context.
   (max-fft-size :accessor max-fft-size :initform 1024)
   (angle-incr :accessor angle-incr :initform 0.002)
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
   (fft-size :accessor fft-size :initform 512)
   (angle :accessor angle :initform 0)
   (display-mode :accessor display-mode :initform 3)
   (follow :accessor follow :initform nil)
   (curr-path-idx :accessor curr-path-idx :initform 0)
   (curr-path-length :accessor curr-path-length :initform 0)
   (zoom :accessor zoom :initform 1)
   (curr-fft-idxs :accessor curr-fft-idxs :initform nil)
   (curr-num :accessor curr-num :initform 512) ;;; length of curr-fft-idxs
   (draw-p :accessor draw-p :initform t)
   (draw-once-p :accessor draw-once-p :initform nil)
   (continuous-p :accessor continuous-p :initform t)
   (draw-shape-p :accessor draw-shape-p :initform t)
   (draw-circles-p :accessor draw-circles-p :initform t)
   (curr-shape-coords :accessor curr-shape-coords :initform nil)
   (x-angle :accessor x-angle :initform 0)
   (y-angle :accessor y-angle :initform 0)
   (z-angle :accessor z-angle :initform 0)
   (translation :accessor translation :initform '(0 -0.5 0))
   (view-port :accessor view-port :initform '(0 0 1000 1000))
   (draw-fn :accessor draw-fn :initform #'draw-time-view)
   (calc-offs-fn :accessor calc-offs-fn :initform #'gl-set-offset-vectors-time)
   (zero-z :accessor zero-z :initform t)
   (movement :accessor movement :initform (make-movement))
   (modifiers :accessor modifiers :initform nil)
   (shape :accessor shape :type (or null ) :initform nil)
   (mouse-start-x-angle :accessor mouse-start-x-angle :initform 0)
   (mouse-start-y-angle :accessor mouse-start-y-angle :initform 0)
   (mouse-start-z-angle :accessor mouse-start-z-angle :initform 0)
   (mouse-start-x :accessor mouse-start-x :type float :initform 0.0)
   (mouse-start-y :accessor mouse-start-y :type float :initform 0.0)) 
  (:default-initargs :width 400 :height 400 :pos-x 100 :pos-y 100
		     :mode '(:double :rgb :depth) :title "02-circles.lisp"))


(defun move-real (window)
 (setf (movement window)
       (make-movement
        :target-x-angle 0
        :target-y-angle 90
        :target-z-angle 0
        :active t)))

(defun move-imag (window)
 (setf (movement window)
       (make-movement
        :target-x-angle 90
        :target-y-angle 90
        :target-z-angle 0
        :active t)))

(defun move-default (window)
  (setf (movement window)
        (make-movement
         :target-x-angle 0
         :target-y-angle 0
         :target-z-angle 0
         :active t)))

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
  (destructuring-bind (min-x max-x min-y max-y min-z max-z) range
    (apply #'vector
           (append
            (list 0.0 0.0 min-z 0.0 0.0 max-z)
            (loop
              for z from min-z to max-z by 0.5
              for l = (< (- z (round z)) 0.01)
              append (let ((len (if l 0.04 0.02)))
                       (list
                        (* len -1) 0.0 (float z 1.0)
                        len 0.0 (float z 1.0))))
            (loop
              for z from min-z to max-z by 0.5
              for l = t then (not l)
              append (let ((len (if l 0.04 0.02)))
                       (list
                        0.0 (* len -1) (float z 1.0)
                        0.0 len (float z 1.0))))
            (loop
              for x from min-x to max-x by distance
              append (list
                      (float x 1.0) (float min-y 1.0) 0.0
                      (float x 1.0) (float max-y 1.0) 0.0))
            (loop
              for y from min-y to max-y by distance
              append (list
                      (float min-x 1.0) (float y 1.0) 0.0
                      (float max-x 1.0) (float y 1.0) 0.0))))))

;;; (get-grid-verts 10)

(defmacro set-vbo-data ((type mode buffer) verts)
  `(let* ((arr (gl:alloc-gl-array :float (length ,verts))))
     (dotimes (i (length ,verts))
       (setf (gl:glaref arr i) (float (elt ,verts i) 1.0)))
     (with-bound-buffer (,type ,buffer)
       (gl:buffer-data ,type ,mode arr))
     (gl:free-gl-array arr)))

(defun get-idx (i display-mode shape)
  (with-slots (size fft-idx-sorted) shape
    (case display-mode
      (3 (elt fft-idx-sorted (mirror-list i size)))
      (2 (elt fft-idx-sorted i))
      (1 (mod (+ i *shift*) size))
      (otherwise (mirror-list2 i size)))))

(defmacro gl-enqueue (closure window)
  `(setf (gl-queue ,window)
         (append (list (lambda () ,closure))
                 (gl-queue ,window))))


(defun gl-set-fft-idxs (fft-idxs window)
  (setf (curr-fft-idxs window) fft-idxs)
  (setf (curr-num window) (length fft-idxs))
  (gl-calc-curr-shape-coords window)
  (setf (draw-once-p window) t))

(defun gl-calc-curr-shape-coords (window)
  "set shape-vbo coords according to the curr-fft-idxs."
;;  (format t "recalc-shape~%")
  (with-slots (curr-fft-idxs shape shape-vbo) window
    (with-slots (fft fft-size) shape
        (with-bound-buffer (:array-buffer shape-vbo)
          (gl:with-mapped-buffer (p1 :array-buffer :read-write)
            (loop
              with freq-idx-transform-fn = (get-transform-fn fft-size)
              with first-coord = (loop
                                   for fft-idx in curr-fft-idxs
                                   for freq-idx = (funcall freq-idx-transform-fn fft-idx)
                                   for offs = (* *gl-scale* (aref fft fft-idx)) summing offs)
              for coord = first-coord then next-coord
              for i below fft-size
              for next-angle = (* 2 pi (/ (1+ i) fft-size))
              for next-coord = (loop
                                 for fft-idx in curr-fft-idxs
                                 for freq-idx = (funcall freq-idx-transform-fn fft-idx)
                                 for offs = (* *gl-scale* (exp (* +i+ freq-idx next-angle))
                                               (aref fft fft-idx))
                                 summing offs)
              do (let ((offs (* 6 i)))
                   (setf (cffi:mem-aref p1 :float offs) (float (realpart coord) 1.0))
                   (setf (cffi:mem-aref p1 :float (+ offs 1)) (float (imagpart coord) 1.0))
                   (setf (cffi:mem-aref p1 :float (+ offs 2)) (float (/ i fft-size) 1.0))
                   (setf (cffi:mem-aref p1 :float (+ offs 3)) (float (realpart next-coord) 1.0))
                   (setf (cffi:mem-aref p1 :float (+ offs 4)) (float (imagpart next-coord) 1.0))
                   (setf (cffi:mem-aref p1 :float (+ offs 5)) (float (/ (1+ i) fft-size) 1.0)))))))))

;;; (gl-enqueue (gl-set-shape *violinschluessel-512* *window*) *window*)
;;; (gl-enqueue (gl-set-shape *achtel-512* *window*) *window*)
;;; (gl-enqueue (gl-set-shape *sawtooth-8* *window*) *window*)

(defun gl-set-shape (new-shape window)
  (declare (special window)) ;;; this function gets called with window bound to the current window
  (if (<= (shape-fft-size new-shape) (max-fft-size window))
      (with-slots (shape fft-size curr-num angle curr-path-length curr-fft-idxs) window
        (setf shape new-shape)
        (setf curr-fft-idxs (coerce (shape-fft-idx-sorted shape) 'list))
        (setf curr-num (min (length curr-fft-idxs) (shape-fft-size (shape window))))
        (setf fft-size (shape-fft-size (shape window)))
        (setf angle 0)
        (setf curr-path-length 0)
        (gl-calc-curr-shape-coords window)
        (clear-shape window))
      (warn "fft-size of shape too big. To use this shape restart program with :max-fft-size set to at least ~a."
            (shape-fft-size new-shape))))

(defun set-zero-time-axis (window)
  (with-bound-buffer (:array-buffer (grid-vbo window))
    (gl:with-mapped-buffer (p3 :array-buffer :read-write)
      (loop
        for idx from 0
        for coordinate in (append
                           (list 0.0 0.0 0.0 0.0 0.0 7.0)
                           (loop
                             for z from 0 to 8 by 0.5
                             for l = (< (- z (round z)) 0.01)
                             append (let ((len (if l 0.04 0.02)))
                                      (list
                                       (* len -1) 0.0 (float z 1.0)
                                       len 0.0 (float z 1.0))))
                           (loop
                             for z from 0 to 8 by 0.5
                             for l = t then (not l)
                             append (let ((len (if l 0.04 0.02)))
                                      (list
                                       0.0 (* len -1) (float z 1.0)
                                       0.0 len (float z 1.0)))))
        do (setf (cffi:mem-aref p3 :float idx) (float coordinate 1.0))))))

(defun set-neg-freq-axis (window)
  (with-bound-buffer (:array-buffer (grid-vbo window))
    (gl:with-mapped-buffer (p3 :array-buffer :read-write)
      (loop
        for idx from 0
        for coordinate in (append
                           (list 0.0 0.0 -3.5 0.0 0.0 3.5)
                           (loop
                             for z from -4 to 4 by 0.5
                             for l = (< (- z (round z)) 0.01)
                             append (let ((len (if l 0.04 0.02)))
                                      (list
                                       (* len -1) 0.0 (float (* z 1/4 pi) 1.0)
                                       len 0.0 (float (* z 1/4 pi) 1.0))))
                                                      (loop
                             for z from -4 to 4 by 0.5
                             for l = t then (not l)
                             append (let ((len (if l 0.04 0.02)))
                                      (list
                                       0.0 (* len -1) (float (* z 1/4 pi) 1.0)
                                       0.0 len (float (* z 1/4 pi) 1.0)))))
        do (setf (cffi:mem-aref p3 :float idx) (float coordinate 1.0))))))

(defun set-freq-axis (window)
  (with-bound-buffer (:array-buffer (grid-vbo window))
    (gl:with-mapped-buffer (p3 :array-buffer :read-write)
      (loop
        for idx from 0
        for coordinate in (append
                           (list 0.0 0.0 0 0.0 0.0 7)
                           (loop
                             for z from 0 to 8 by 0.5
                             for l = (< (- z (round z)) 0.01)
                             append (let ((len (if l 0.04 0.02)))
                                      (list
                                       (* len -1) 0.0 (float (* z 1/4 pi) 1.0)
                                       len 0.0 (float (* z 1/4 pi) 1.0))))
                                                      (loop
                             for z from 0 to 8 by 0.5
                             for l = t then (not l)
                             append (let ((len (if l 0.04 0.02)))
                                      (list
                                       0.0 (* len -1) (float (* z 1/4 pi) 1.0)
                                       0.0 len (float (* z 1/4 pi) 1.0)))))
        do (setf (cffi:mem-aref p3 :float idx) (float coordinate 1.0))))))


#|
(defun set-offset-data (w)
  (with-slots (offset-vbo num) w
    (set-vbo-data (:array-buffer :dynamic-draw offset-vbo)
                  (loop for i below fft-size append (list (- (* 0.2 (mod i 10)) 0.9)
                                                     (- (* 0.2 (mod (floor i 10) 10)) 0.9)
                                                     (+ 0.1 (random 0.7))
                                                     0.0)))))
|#



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
               max-fft-size)
      window

    
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
      (let ((verts (get-grid-verts '(-5 5 -5 5 -4 4) (* *gl-scale* 0.125))))
        (set-vbo-data (:array-buffer :static-draw grid-vbo) verts)
        (setf  grid-vbo-size (/ (length verts) 6)))
      (set-vbo-data (:array-buffer :static-draw circle-vbo)
                    (get-circle-verts 64 (* *gl-scale* 0.1)))
      (set-vbo-data (:array-buffer :static-draw arrowhead-vbo)
                    (apply #'vector (mapcar (lambda (v) (* v *gl-scale*)) *arrowhead*)))
      (set-vbo-data (:array-buffer :static-draw arrowstem-vbo)
                    (apply #'vector (mapcar (lambda (v) (* v *gl-scale*)) '(0.0 0.002 0.08 0.002 0.08 -0.002 0.0 -0.002))))
      (set-vbo-data (:array-buffer :dynamic-draw shape-vbo)
                    (loop for i below (* 2 max-fft-size) append (list 0.0 0.0 0.0)))
      (set-vbo-data (:array-buffer :dynamic-draw offset-vbo)
                    (loop for i below max-fft-size append (list 0.0 0.0 0.0 0.0)))
      (set-vbo-data (:array-buffer :dynamic-draw angle-vbo)
                    (loop for i below max-fft-size collect 0.0))

      
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

(defun toggle-draw (w)
  (setf (draw-p w) (not (draw-p w))))

(defun draw-shape (proj-mat program vao idx num)
  (with-program (program)
    (gl:line-width 2)
    (cl-opengl-bindings:uniform-4f (gl:get-uniform-location program "Color") 1.0 1.0 0.0 1.0)
    (gl:uniform-matrix 
     (gl:get-uniform-location program "projection") 4 (vector proj-mat) nil)
    (with-bound-vertex-array (vao)
      (gl:draw-arrays :lines (* 2 idx) (* 2 num)))))

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
    (with-bound-vertex-array (arrowhead-vao)
      (gl:draw-arrays-instanced :triangles 0 3 num))
    (with-bound-vertex-array (arrowstem-vao)
      (gl:draw-arrays-instanced :quads 0 4 num))))

(defun clear-shape (window)
  (with-slots (curr-path-length curr-path-idx angle fft-size draw-once-p) window
    (setf curr-path-idx 0)
    (setf angle 0)
    (setf curr-path-length 0)
    (setf draw-once-p t)))

;;(clear-shape *window*)

(defparameter *print* nil)

(defun adjust-camera (w)
  (with-slots (movement) w
    (with-slots (numsteps curr-step active) movement
      (dolist (pair '((x-angle target-x-angle)
                      (y-angle target-y-angle)
                      (z-angle target-z-angle)
                      (zoom target-zoom)
                      (translation target-translation)))
        (if (slot-value movement (second pair))
            (setf (slot-value w (first pair))
                  (iinterpl (slot-value w (first pair))
                            (slot-value movement (second pair))
                            curr-step numsteps))))
      (if (= (incf curr-step) numsteps) (setf active nil)))))

(defun gl-set-offset-vectors-zero (window)
  (let ((result 0))
    (with-slots (offset-vbo angle curr-fft-idxs shape display-mode) window
      (with-slots (freq-idx-transform-fn fft) shape
        (with-bound-buffer (:array-buffer offset-vbo)
          (gl:with-mapped-buffer (p1 :array-buffer :read-write)
            (loop
              for curr-offset = (complex 0.0 0.0) then next-offset
              for i from 0
              for x in curr-fft-idxs
              for freq-idx = (funcall freq-idx-transform-fn x)
              for next-offset = (+ curr-offset (* *gl-scale* (exp (* +i+ freq-idx angle)) (aref fft x)))
              do (let ((offs (* i 4)))
                   (setf (cffi:mem-aref p1 :float (+ offs 0))
                         (float (realpart curr-offset) 1.0))
                   (setf (cffi:mem-aref p1 :float (+ offs 1))
                         (float (imagpart curr-offset) 1.0))
                   0.01
                   (setf (cffi:mem-aref p1 :float (+ offs 3))
                         (float (* 10 (abs (aref fft x))) 1.0)))
              finally (setf result next-offset))))))
    (values result)))

(defun gl-set-offset-vectors-time (window)
  (let ((result 0))
    (with-slots (offset-vbo angle curr-fft-idxs shape display-mode) window
      (with-slots (freq-idx-transform-fn fft) shape
        (with-bound-buffer (:array-buffer offset-vbo)
          (gl:with-mapped-buffer (p1 :array-buffer :read-write)
            (loop
              for curr-offset = (complex 0.0 0.0) then next-offset
              for i from 0
              for x in curr-fft-idxs
              for freq-idx = (funcall freq-idx-transform-fn x)
              for next-offset = (+ curr-offset (* *gl-scale* (aref fft x) (exp (* +i+ freq-idx angle))))
              do (let ((offs (* i 4)))
                   (setf (cffi:mem-aref p1 :float (+ offs 0))
                         (float (realpart curr-offset) 1.0))
                   (setf (cffi:mem-aref p1 :float (+ offs 1))
                         (float (imagpart curr-offset) 1.0))
                   (setf (cffi:mem-aref p1 :float (+ offs 2))
                         (float angle 1.0))
                   (setf (cffi:mem-aref p1 :float (+ offs 3))
                         (float (* 10 (abs (aref fft x))) 1.0)))
              finally (setf result next-offset))))))
    (values result)))

(defun gl-set-offset-vectors-neg-spectrum (window)
  (let ((result 0))
    (with-slots (offset-vbo angle curr-fft-idxs shape display-mode fft-size) window
      (with-slots (freq-idx-transform-fn fft) shape
        (with-bound-buffer (:array-buffer offset-vbo)
          (gl:with-mapped-buffer (p1 :array-buffer :read-write)
            (loop
              for curr-offset = (complex 0.0 0.0) then next-offset
              for i from 0
              for x in curr-fft-idxs
              for freq-idx = (funcall freq-idx-transform-fn x)
              for next-offset = (+ curr-offset (* *gl-scale* (exp (* +i+ freq-idx angle)) (aref fft x)))
              do (let ((offs (* i 4)))
                   (setf (cffi:mem-aref p1 :float (+ offs 0))
                         0.0)
                   (setf (cffi:mem-aref p1 :float (+ offs 1))
                         0.0)
                   (setf (cffi:mem-aref p1 :float (+ offs 2))
                         (float (* (/ freq-idx fft-size) (* 2 pi)) 1.0))
                   (setf (cffi:mem-aref p1 :float (+ offs 3))
                         (float (* 10 (abs (aref fft x))) 1.0)))
              finally (setf result next-offset))))))
    (values result)))

(defun gl-set-offset-vectors-spectrum (window)
  (let ((result 0))
    (with-slots (offset-vbo angle curr-fft-idxs shape display-mode fft-size) window
      (with-slots (freq-idx-transform-fn fft) shape
        (with-bound-buffer (:array-buffer offset-vbo)
          (gl:with-mapped-buffer (p1 :array-buffer :read-write)
            (loop
              for curr-offset = (complex 0.0 0.0) then next-offset
              for i from 0
              for x in curr-fft-idxs
              for freq-idx = x
              for next-offset = (+ curr-offset (* *gl-scale* (exp (* +i+ freq-idx angle)) (aref fft x)))
              do (let ((offs (* i 4)))
                   (setf (cffi:mem-aref p1 :float (+ offs 0))
                         0.0)
                   (setf (cffi:mem-aref p1 :float (+ offs 1))
                         0.0)
                   (setf (cffi:mem-aref p1 :float (+ offs 2))
                         (float (* (/ freq-idx fft-size) (* 2 pi)) 1.0))
                   (setf (cffi:mem-aref p1 :float (+ offs 3))
                         (float (* 10 (abs (aref fft x))) 1.0)))
              finally (setf result next-offset))))))
    (values result)))

#|
(defun gl-set-offset-vectors-spectrum (window)
  (let ((result 0))
    (with-slots (offset-vbo angle curr-fft-idxs shape display-mode fft-size) window
      (with-slots (freq-idx-transform-fn fft) shape
        (with-bound-buffer (:array-buffer offset-vbo)
          (gl:with-mapped-buffer (p1 :array-buffer :read-write)
            (loop
              for curr-offset = (complex 0.0 0.0) then next-offset
              for i from 0
              for x in curr-fft-idxs
              for freq-idx = (funcall freq-idx-transform-fn x)
              for next-offset = (+ curr-offset (* *gl-scale* (exp (* +i+ freq-idx angle)) (aref fft x)))
              do (let ((offs (* i 4)))
                   (setf (cffi:mem-aref p1 :float (+ offs 0))
                         0.0)
                   (setf (cffi:mem-aref p1 :float (+ offs 1))
                         0.0)
                   (setf (cffi:mem-aref p1 :float (+ offs 2))
                         (if (< x (/ fft-size 2))
                             (float (* (/ x fft-size) (* 2 pi)) 1.0)
                             (float (- (* (/ x fft-size) (* 2 pi)) (* 2 pi)) 1.0)))
                   (setf (cffi:mem-aref p1 :float (+ offs 3))
                         (float (* 10 (abs (aref fft x))) 1.0)))
              finally (setf result next-offset))))))
    (values result)))
|#

(defmethod glut:display ((w circle-window))
  (update-swank)
  (let ((curr-pos))
    ;; (gl:clear :color-buffer-bit :depth-buffer-bit)
    (with-slots (angle
                 circle-program circle-vao
                 arrow-program arrowhead-vao arrowstem-vao
                 shape-program shape-vao grid-vao grid-vbo-size
                 offset-vbo angle-vbo shape-vbo
                 shape curr-fft-idxs zoom
                 fft-size
                 curr-path-idx
                 curr-path-length
                 x-angle y-angle z-angle
                 translation movement
                 last-pos angle-incr
                 display-mode follow
                 view-port draw-fn calc-offs-fn
                 gl-queue continuous-p) 
        w
      (continuable
        (let ((draw-discrete-p nil))
           (gl-init)
           (loop while gl-queue do (funcall (pop gl-queue)))
           (if (or (draw-p w) (draw-once-p w))
               (progn
                 (gl:bind-buffer :array-buffer offset-vbo)
                 (with-slots (freq-idx-transform-fn scale fft) shape
                   (let  ((tmp curr-path-idx))
                     (setf curr-path-idx (floor (* (/ angle (* 2 pi)) fft-size)))
                     (setf curr-path-length (max curr-path-length (1+ curr-path-idx)))
                     (if (or continuous-p (/= tmp curr-path-idx))
                         (setf curr-pos (funcall calc-offs-fn w))
                         (setf curr-pos last-pos))
;;;                (format t "~a, ~a ~a; " curr-path-length tmp curr-path-idx)
                     (when continuous-p
                         (with-bound-buffer (:array-buffer shape-vbo)
                           (gl:with-mapped-buffer (p3 :array-buffer :read-write)
                             (when (/= tmp curr-path-idx)
                               (setf draw-discrete-p t)
                               (let ((prev-offs (* tmp 6))
                                     (next-offs (* (mod (1+ tmp) fft-size) 6)))
                                 (setf (cffi:mem-aref p3 :float (+ prev-offs 3))
                                       (cffi:mem-aref p3 :float (+ next-offs 0)))
                                 (setf (cffi:mem-aref p3 :float (+ prev-offs 4))
                                       (cffi:mem-aref p3 :float (+ next-offs 1)))
                                 (setf (cffi:mem-aref p3 :float (+ prev-offs 5))
                                       (float (/ (1+ tmp) fft-size) 1.0))))
                             (let ((offs (* curr-path-idx 6)))
                               (setf (cffi:mem-aref p3 :float (+ offs 3)) (float (realpart curr-pos) 1.0))
                               (setf (cffi:mem-aref p3 :float (+ offs 4)) (float (imagpart curr-pos) 1.0))
                               (setf (cffi:mem-aref p3 :float (+ offs 5)) (float (/ (max 0 angle) (* 2 pi)) 1.0))))))
                     (when (or continuous-p (/= tmp curr-path-idx))
                       (with-bound-buffer (:array-buffer angle-vbo)
                         (gl:with-mapped-buffer (p2 :array-buffer :read-write)
                           (loop
                             for i from 0
                             for x in curr-fft-idxs
                             with tmp-angle = (if continuous-p angle (* 2 pi (/ (floor (* fft-size (/ angle (* 2 pi)))) fft-size)))
                             for freq-idx = (funcall freq-idx-transform-fn x)
                             do (progn
                                  (setf (cffi:mem-aref p2 :float i) (float (+ (phase (aref fft x)) (* tmp-angle freq-idx)) 1.0))
                                  ;;                 (setf (cffi:mem-aref p2 :float i) (float (elt point 3) 1.0))
                                  (if *print* (format t " ~a~%"
                                                      (cffi:mem-aref p2 :float i))))))))))
                 (setf angle (mod (+ angle angle-incr) (* 2 pi)))
                 (setf last-pos curr-pos)))
           (gl:matrix-mode :modelview)
           (gl:load-identity)
;;;            (glu:perspective 50 (/ (glut:width w) (glut:height w)) -10 1)
            (apply #'gl:viewport view-port)
           (apply #'gl:translate '(0 -0.45 0))
           (if (movement-active movement)
               (adjust-camera w))
           (gl:scale (/ zoom 5) (/ zoom 5) (/ zoom 5))
           (gl:rotate x-angle 1 0 0)
           (gl:rotate y-angle 0 1 0)
           (gl:rotate z-angle 0 0 1)
           (apply #'gl:translate translation)

           (if follow
               (gl:translate (* -1 (realpart curr-pos)) (* -1 (imagpart curr-pos)) (* -1 angle)))
;;;            (gl:translate 0 0 (* -1 (/ angle (* 2 pi))))
          (when circle-program
             (funcall draw-fn w))
           (setf (draw-once-p w) nil)))))
  (glut:swap-buffers)
  (gl:finish))

(defun crossing? (val1 val2 num)
  (/= (floor val1 (/ num))
      (floor val2 (/ num))))

(defun draw-time-view-zero (window)
  (with-slots (shape-program grid-vao shape-vao
               curr-path-idx curr-path-length
               circle-program circle-vao
               arrow-program arrowhead-vao arrowstem-vao
               grid-vbo-size angle curr-num fft-size)
      window
    (let ((proj-mat (gl:get-float :modelview-matrix)))
      (draw-grid proj-mat shape-program grid-vao grid-vbo-size)
      (draw-circles (gl:get-float :modelview-matrix) circle-program circle-vao curr-num)
      (draw-arrows (gl:get-float :modelview-matrix) arrow-program arrowhead-vao arrowstem-vao curr-num)
      (translate 0 0 (+ -0.0 (* 6.28 angle 0.159)))
      (scale 1 1 6.28)
      (draw-shape (gl:get-float :modelview-matrix) shape-program shape-vao 0 curr-path-length)
      (if (>= curr-path-length fft-size)
          (progn
            (translate 0 0 1 )
            (draw-shape (gl:get-float :modelview-matrix) shape-program shape-vao 0 curr-path-idx))))))

;; (setf (draw-fn *window*) #'draw-time-view)


(defun draw-time-view (window)
  (with-slots (shape-program grid-vao shape-vao
               curr-path-idx curr-path-length
               circle-program circle-vao
               arrow-program arrowhead-vao arrowstem-vao
               grid-vbo-size angle curr-num fft-size
               draw-shape-p draw-circles-p
               shape)
      window
    (with-slots (scale) shape
      (let ((proj-mat (gl:get-float :modelview-matrix)))
        (draw-grid proj-mat shape-program grid-vao grid-vbo-size)
        (scale scale scale 1)
        (when draw-circles-p (draw-circles (gl:get-float :modelview-matrix) circle-program circle-vao curr-num))
        (draw-arrows (gl:get-float :modelview-matrix) arrow-program arrowhead-vao arrowstem-vao curr-num)
        (translate 0 0 0)
        (scale 1 1 6.28)
        (when draw-shape-p
          (draw-shape (gl:get-float :modelview-matrix) shape-program shape-vao 0 curr-path-length))))))

(defun draw-spectrum-view (window)
    (with-slots (shape-program grid-vao shape-vao
                 curr-path-idx curr-path-length
                 circle-program circle-vao
                 arrow-program arrowhead-vao arrowstem-vao
                 grid-vbo-size angle curr-num fft-size
                 draw-shape-p draw-circles-p
                 shape)
        window
      (with-slots (scale) shape
        (let ((proj-mat (gl:get-float :modelview-matrix)))
          (draw-grid proj-mat shape-program grid-vao grid-vbo-size)
          (scale scale scale 1)
          (when draw-circles-p (draw-circles (gl:get-float :modelview-matrix) circle-program circle-vao curr-num))
          (draw-arrows (gl:get-float :modelview-matrix) arrow-program arrowhead-vao arrowstem-vao curr-num)
          (translate 0 0 0)
          (scale 1 1 6.28)
          (when draw-shape-p (draw-shape (gl:get-float :modelview-matrix) shape-program shape-vao 0 curr-path-length))))))

(defun gl-set-mode (mode window)
  (with-slots (draw-fn calc-offs-fn draw-shape-p draw-circles-p angle draw-p draw-once-p) window
    (case mode
      (1 (progn
           (set-zero-time-axis window)
           (setf draw-fn #'draw-time-view)
           (setf calc-offs-fn #'gl-set-offset-vectors-time)
           (setf draw-circles-p t)
           (setf draw-shape-p t)
           (setf draw-once-p t)))
      (2 (progn
           (set-neg-freq-axis window)
           (setf draw-fn #'draw-spectrum-view)
           (setf calc-offs-fn #'gl-set-offset-vectors-neg-spectrum)
           (setf draw-circles-p t)
           (setf draw-shape-p nil)
           (setf draw-once-p t)))
      (3 (progn
           (setf draw-p nil)
           (set-neg-freq-axis window)
           (setf draw-fn #'draw-spectrum-view)
           (setf calc-offs-fn #'gl-set-offset-vectors-neg-spectrum)
           (setf draw-circles-p nil)
           (setf draw-shape-p nil)
           (setf angle 0)
           (setf draw-once-p t)))
      (4 (progn
           (set-freq-axis window)
           (setf draw-fn #'draw-spectrum-view)
           (setf calc-offs-fn #'gl-set-offset-vectors-spectrum)
           (setf draw-circles-p t)
           (setf draw-shape-p nil)
           (setf draw-once-p t)))
      (5 (progn
           (setf draw-p nil)
           (set-freq-axis window)
           (setf draw-fn #'draw-spectrum-view)
           (setf calc-offs-fn #'gl-set-offset-vectors-spectrum)
           (setf draw-circles-p nil)
           (setf draw-shape-p nil)
           (setf angle 0)
           (setf draw-once-p t))))))

(defmethod glut:reshape ((w circle-window) width height)
  (with-slots (circle-program arrow-program shape-program view-port translation) w
    (setf view-port (list (* -0.0 (max width height)) (* 0 (max width height)) (* 1 (max width height)) (* 1 (max width height))))
    (setf translation (list 0 0 0))
    (apply #'gl:viewport view-port)
    (gl:matrix-mode :modelview)
    ;; Ensure that projection matrix ratio always matches the window size ratio,
    ;; so the polygon will always look square.
    (gl:load-identity)
;;; (glu:perspective 50 (/ (glut:width window) (glut:height window)) -1 1)
;;; (gl:ortho 0 width 0 height -1 1)
;;; (gl:translate (* (- 1 gl-scale) gl-width) (* (- 1 gl-scale) gl-height) 0.0)
;;;    (gl:scale 1 1 1)
;;;    (gl:ortho -1 1 -1 1 -1 1)

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
    (with-slots (modifiers) window
      (setf modifiers (glut:get-modifiers))
      (case button
        (:left-button
         (when (and (null modifiers) (eql state :down))
           (let ()
             (setf (mouse-start-x window) x)
             (setf (mouse-start-y window) y)
;;;             (format t "~a, ~a, ~a~%" (x-angle window) (y-angle window) (z-angle window))
             (setf (mouse-start-x-angle window) (x-angle window))
             (setf (mouse-start-y-angle window) (y-angle window))
             (setf (mouse-start-z-angle window) (z-angle window)))))
        (:wheel-down (if (eql state :down)
                         (cond
                           ((equal (list :active-ctrl) modifiers)
                            (setf (zoom window) (* (zoom window) 95/100)))
                           ((null modifiers)
                            (setf (angle-incr window) (* (angle-incr window) 95/100))))))
        (:wheel-up (if (eql state :down)
                       (cond
                         ((equal (list :active-ctrl) modifiers)
                          (setf (zoom window) (* (zoom window) 100/95)))
                         ((null (glut:get-modifiers))
                          (setf (angle-incr window) (* (angle-incr window) 100/95))))))
        (:right-button (if (eql state :down)
                           (cond
                             ((equal (list :active-ctrl) modifiers)
                              (setf (zoom window) 1.0))
                             ((null modifiers) (move-default window))
                             ((equal (list :active-shift) modifiers)
                              (move-real window))
                             ((and (member :active-shift modifiers)
                                   (member :active-alt modifiers))
                              (move-imag window)))))))))

(defmethod glut:motion ((window circle-window) x y)
  (continuable
    (with-slots (modifiers) window
      (let* ((scale (/ 180 (min (glut:height window) (glut:width window))))
             (dx (float (* (- x (mouse-start-x window)) scale)))
             (dy (float (* (- y (mouse-start-y window)) scale))))
;;        (format t "~a ~a ~a ~a, ~a~%" x (mouse-start-x window) dx dy modifiers)
        (if (equal (list :active-shift) modifiers)
            (progn
;;              (setf (y-angle window) (+ (mouse-start-y-angle window) dx))
              (setf (y-angle window) (+ (mouse-start-y-angle window) dx)))
            (setf (y-angle window) (+ (mouse-start-y-angle window) dx)))
        (setf (x-angle window) (+ (mouse-start-x-angle window) dy))))))

(defmethod glut:keyboard ((w circle-window) key x y)
  (declare (ignore x y))
  (case key
    (#\Esc (glut:destroy-current-window))
    (#\f (setf (follow w) (not (follow w))))
    (#\c (clear-shape w))
    (#\u (setf (draw-once-p w) t))
    (#\a (setf (zero-z w) (not (zero-z w)))
     (if (zero-z w)
         (set-zero-time-axis w)
         (set-freq-axis w)))
    (#\SPACE (toggle-draw w))
    (#\q (setf (continuous-p w) (not (continuous-p w))))
    (#\1 (format t "mode: 1~%") (gl-set-mode 1 w))
    (#\2 (format t "mode: 2~%") (gl-set-mode 2 w))
    (#\3 (format t "mode: 3~%") (gl-set-mode 3 w))
    (#\4 (format t "mode: 4~%") (gl-set-mode 4 w))
    (#\5 (format t "mode: 5~%") (gl-set-mode 5 w))
))

;; Cleanup.
;; Most of the objects we created have analogous deletion function.
(defmethod glut:close ((w circle-window))
  (when (slot-boundp w 'circle-program)
   (gl:delete-program (circle-program w))))

(defun 02-indexed-circles (&key (max-fft-size 1024))
  (let ((w (make-instance 'circle-window :width 1000 :height 800 :max-fft-size max-fft-size)))
    (setf *window* w)
    (gl-enqueue (gl-set-shape *achtel-512* w) w)
    (gl-enqueue (set-zero-time-axis w) w)
    (unwind-protect
         (continuable
           (glut:display-window w))
      (when (not (glut::destroyed w))
         (setf (glut::destroyed w) t)
         (glut:destroy-window (glut:id w))))))



;;; (02-indexed-circles)
;;; (gl:named-buffer-storage)

;; (setf (curr-num *window*) 30)

#|
(setf (continuous-p *window*) t)
(setf (continuous-p *window*) nil)
(gl-enqueue (gl-set-shape *sawtooth-512* *window*) *window*)

(gl-enqueue (gl-set-shape *pulse-512* *window*) *window*)

(gl-enqueue (gl-set-fft-idxs '(508) *window*) *window*)

(gl-enqueue (gl-set-fft-idxs '(4) *window*) *window*)

(gl-enqueue (gl-set-fft-idxs '(4 508) *window*) *window*)

(gl-enqueue (gl-set-fft-idxs '(1 511) *window*) *window*)

(subseq (shape-fft-idx-sorted *sawtooth-512*) 0 4)



(setf (continuous-p *window*) nil)

(gl-enqueue (gl-set-shape *sin-pulse-8* *window*) *window*)

(gl-enqueue (gl-set-fft-idxs '(0) *window*) *window*)
(gl-enqueue (gl-set-fft-idxs '(1) *window*) *window*)
(gl-enqueue (gl-set-fft-idxs '(2) *window*) *window*)
(gl-enqueue (gl-set-fft-idxs '(3) *window*) *window*)
(gl-enqueue (gl-set-fft-idxs '(4) *window*) *window*)
(gl-enqueue (gl-set-fft-idxs '(5) *window*) *window*)
(gl-enqueue (gl-set-fft-idxs '(6) *window*) *window*)
(gl-enqueue (gl-set-fft-idxs '(7) *window*) *window*)
(gl-enqueue (gl-set-fft-idxs '(1 7) *window*) *window*)
(gl-enqueue (gl-set-fft-idxs '(1 2 6 7) *window*) *window*)
(gl-enqueue (gl-set-fft-idxs '(1 2 3 4 5 6 7 0) *window*) *window*)

(gl-enqueue (gl-set-fft-idxs '(10 502) *window*) *window*)


(gl-enqueue (gl-set-shape *sin-pulse-8* *window*) *window*)
(gl-enqueue (gl-set-shape *cos-pulse-512* *window*) *window*)
(gl-enqueue (gl-set-shape *violinschluessel-512* *window*) *window*)
(gl-enqueue (gl-set-shape *sawtooth-8* *window*) *window*)
(gl-enqueue (gl-set-shape *sawtooth-512* *window*) *window*)

(gl-enqueue (gl-set-shape *violinschluessel-512* *window*) *window*)
(gl-enqueue (gl-set-shape *achtel-512* *window*) *window*)

(setf (calc-offs-fn *window*) #'gl-set-offset-vectors-spectrum)
(setf (draw-fn *window*) #'draw-spectrum-view)

(curr-path-length *window*)

(last-pos *window*)
(02-indexed-circles)


(setf *print* t)
(setf *print* nil)


|#
