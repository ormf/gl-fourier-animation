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
(defparameter *debug* t)
(defparameter *angle-incr* 0.002)
(defparameter *window* nil)

(defparameter *arrowhead* #(0.08 0.0075 0.1 0.0 0.08 -0.0075))

;;; triangle.lisp --- Example usage of vertex and fragment shaders,
;;; vertex buffer objects, and vertex array objects

(defmacro with-program ((program) &body body)
  `(progn
     (gl:use-program ,program)
     ,@body
     (gl:use-program 0)))

(defclass circle-window (glut:window)
  ((circle-vbo :accessor circle-vbo)
   (arrowhead-vbo :accessor arrowhead-vbo)
   (arrowstem-vbo :accessor arrowstem-vbo)
   (shape-vbo :accessor shape-vbo)
   (coordinate-system-vbo :accessor coordinate-system-vbo)
   (offset-buffer :accessor offset-buffer)
   (angle-buffer :accessor angle-buffer)
   (shape-buffer :accessor shape-buffer)
   (circle-vao :accessor circle-vao)
   (arrowhead-vao :accessor arrowhead-vao)
   (arrowstem-vao :accessor arrowstem-vao)
   (circle-program :accessor circle-program)
   (arrow-program :accessor arrow-program)
   (shape-program :accessor shape-program)
   (num :accessor num :initform 512)
   (angle :accessor angle :initform 0)
   (curr-num :accessor curr-num :initform 512)
   (curr-path :accessor curr-path :initform nil)
   (zoom :accessor zoom :initform 1)
   (x-angle :accessor x-angle :initform 0)
   (y-angle :accessor y-angle :initform 0)
   (z-angle :accessor z-angle :initform 0)
   (shape :accessor shape :type (or null ) :initform nil)) 
  (:default-initargs :width 400 :height 400 :pos-x 100 :pos-y 100
		     :mode '(:double :rgb :depth) :title "02-circles.lisp"))

;;; program for drawing the shape: location = 0 contain the 3D
;;; vertexes of the shape, calculated in realtime for each frame in
;;; the glut:display routine.

(defparameter *shape-vertex-shader-program*
  "
#version 330 core

layout (location = 0) in vec3 aPos;

uniform mat4 projection;

void
main()
{
    gl_Position = projection * vec4(aPos.xyz, 1.0);
}
")

;;; program for indexed drawing: location = 0 contain the vertexes of
;;; one circle (precalculated once in the display-window :before
;;; method), location = 1 contain the offsets/length/angle for each
;;; circle to draw, calculated in realtime for each frame in the
;;; glut:display routine:
;;;
;;; aTransform.xy = offset of shape
;;; aTransform.w = angle
;;; aTransform.z = length
;;;
;;; The phase (aTransform.w) isn't used as the circles don't have to
;;; be rotated.
 
(defparameter *circle-vertex-shader-program*
  "
#version 330 core

layout (location = 0) in vec2 aPos; // The vbo of the shape to draw
layout (location = 1) in vec4 aOffsetLength;  // The offsets from offset buffer
// layout (location = 2) in float aAngle;  // The angle (unused here)

uniform mat4 projection;
// uniform mat4 view;
// uniform mat4 model;

void
main()
{
    gl_Position = projection * vec4((aOffsetLength.w*aPos)+aOffsetLength.xy, aOffsetLength.z, 1.0);
}
")

;;; program for indexed drawing: location = 0 contain the vertexes of
;;; one arrow (precalculated once in the display-window :before
;;; method), location = 1 contain the offsets/length/angle for each
;;; arrow to draw, calculated in realtime for each frame in the
;;; glut:display routine:
;;;
;;; aTransform.xyz = offset of shape
;;; aTransform.w = length
;;; aAngle = angle
;;;

(defparameter *arrow-vertex-shader-program*
  "
#version 330 core

layout (location = 0) in vec2 aPos; // The vbo of the shape to draw
layout (location = 1) in vec4 aOffsetLength;  // The offsets from offset buffer
layout (location = 2) in float aAngle;  // The angle

uniform mat4 projection;
// uniform mat4 view;
// uniform mat4 model;

mat2 rotate2d(float _angle){
    return mat2(cos(_angle),-sin(_angle),
                sin(_angle),cos(_angle));
}

void
main()
{
    gl_Position = projection * vec4((aOffsetLength.w*aPos*rotate2d(aAngle))+aOffsetLength.xy, aOffsetLength.z, 1.0);
}
")

;;; (02-indexed-circles)

(defparameter *fragment-shader-program*
  "#version 330 core

out vec4 fColor;

uniform vec4 Color = vec4(1.0, 1.0, 1.0, 1.0);

void main()
{
    fColor = Color;
}
")

(defun get-circle-verts (num radius)
  (apply #'vector ;;; circle around #(0 0)
         (loop for (pt1 pt2) on (loop for x below (1+ num)
                                      for theta = (mod (* 2 pi (/ x num)) num)
                                      collect `(,(float (* radius (cos theta)) 1.0)
                                                ,(float (* radius (sin theta)) 1.0)))
               while pt2
               append (append pt1 pt2))))

(defmacro with-bound-buffer ((type buffer) &body body)
  `(progn
     (gl:bind-buffer ,type ,buffer)
     ,@body
     (gl:bind-buffer ,type 0)))

(defmacro set-vbo-data ((type mode buffer) verts)
  `(let* ((arr (gl:alloc-gl-array :float (length ,verts))))
     (dotimes (i (length ,verts))
       (setf (gl:glaref arr i) (float (elt ,verts i) 1.0)))
     (gl:bind-buffer ,type ,buffer)
     (gl:buffer-data ,type ,mode arr)
     (gl:free-gl-array arr)
     (gl:bind-buffer ,type 0)))

(defun set-offset-data (w)
  (with-slots (offset-buffer num) w
    (set-vbo-data (:array-buffer :dynamic-draw offset-buffer)
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
               circle-vao arrowhead-vao arrowstem-vao
               offset-buffer angle-buffer shape-buffer
               circle-program arrow-program shape-program
               num)
      window


    ;; Vertex array objects manage which vertex attributes are
    ;; associated with which data buffers. 

;;; setup shaders

    (let ((circle-vs (gl:create-shader :vertex-shader))
          (arrow-vs (gl:create-shader :vertex-shader))
          (shape-vs (gl:create-shader :vertex-shader))
          (fs (gl:create-shader :fragment-shader)))
      
      (gl:shader-source circle-vs *circle-vertex-shader-program*)
      (gl:compile-shader circle-vs)
      (gl:shader-source arrow-vs *arrow-vertex-shader-program*)
      (gl:compile-shader arrow-vs)
      (gl:shader-source shape-vs *shape-vertex-shader-program*)
      (gl:compile-shader shape-vs)
      (gl:shader-source fs *fragment-shader-program*)
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
      (format t "circle-program: ~a" circle-program)
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
      
      (let ((buffers (gl:gen-buffers 6)))
        (setf circle-vbo (elt buffers 0))
        (setf arrowhead-vbo (elt buffers 1))
        (setf arrowstem-vbo (elt buffers 2))
        (setf offset-buffer (elt buffers 3))
        (setf angle-buffer (elt buffers 4))
        (setf shape-buffer (elt buffers 5)))
      (set-vbo-data (:array-buffer :static-draw circle-vbo)
                    (get-circle-verts 64 0.1))
      (set-vbo-data (:array-buffer :static-draw arrowhead-vbo) *arrowhead*)
      (set-vbo-data (:array-buffer :static-draw arrowstem-vbo)
                    #(0.0 0.002 0.08 0.002 0.08 -0.002 0.0 -0.002))  
      (set-vbo-data (:array-buffer :dynamic-draw offset-buffer)
                    (loop for i below num append (list (- (* 0.2 (mod i 10)) 0.9)
                                                       (- (* 0.2 (mod (floor i 10) 10)) 0.9)
                                                       0.0
                                                       (+ 0.1 (random 0.7)))))
      (set-vbo-data (:array-buffer :dynamic-draw angle-buffer)
                    (loop for i below num collect 0.0))
      (set-vbo-data (:array-buffer :dynamic-draw shape-buffer)
                    (loop for i below num append (list 0.0 0.0 0.0 0.0)))
      
;;; circle setup indexed rendering:

      (setf circle-vao (gl:gen-vertex-array))
      (gl:bind-vertex-array circle-vao)

      ;; To associate our CIRCLE-VBO data with this VAO, we bind it, specify
      ;; which vertex attribute we want to associate it with, and specify
      ;; where the data comes from.
      (gl:bind-buffer :array-buffer circle-vbo)
      ;; Using a null pointer as the data source indicates that we want
      ;; the vertex data to come from the currently bound array-buffer.
      (gl:enable-vertex-attrib-array 0)
      (gl:vertex-attrib-pointer 0 2 :float nil (* 2 +float-size+) (cffi:null-pointer))

      ;; In this program, we use attribute 0 for position. If you had
      ;; per-vertex normals, you could use a different attribute for those
      ;; as well.
      (gl:bind-buffer :array-buffer 0)

      (gl:bind-buffer :array-buffer offset-buffer)
      (gl:enable-vertex-attrib-array 1)
      (gl:vertex-attrib-pointer 1 4 :float nil (* 4 +float-size+) (cffi:null-pointer))
      (gl:bind-buffer :array-buffer 0)
      (cl-opengl-bindings:vertex-attrib-divisor 1 1)
      (gl:bind-vertex-array 0)

;;; arrowhead setup indexed rendering:
      
      (setf arrowhead-vao (gl:gen-vertex-array))
      (gl:bind-vertex-array arrowhead-vao)

      (gl:bind-buffer :array-buffer arrowhead-vbo)
      (gl:enable-vertex-attrib-array 0)
      (gl:vertex-attrib-pointer 0 2 :float nil (* 2 +float-size+) (cffi:null-pointer))
      (gl:bind-buffer :array-buffer 0)

      (gl:bind-buffer :array-buffer offset-buffer)
      (gl:enable-vertex-attrib-array 1)
      (gl:vertex-attrib-pointer 1 4 :float nil (* 4 +float-size+) (cffi:null-pointer))
      (gl:bind-buffer :array-buffer 0)
      
      (gl:bind-buffer :array-buffer angle-buffer)
      (gl:vertex-attrib-pointer 2 1 :float nil
                                (* 1 +float-size+) (cffi:null-pointer)) ;;; idx corresponds to position in Vertex shader code
      (gl:enable-vertex-attrib-array 2) ;;; idx corresponds to position in Vertex shader code
      (gl:bind-buffer :array-buffer 0)
      (cl-opengl-bindings:vertex-attrib-divisor 1 1)
      (cl-opengl-bindings:vertex-attrib-divisor 2 1)
      (gl:bind-vertex-array 0)
    
      (setf arrowstem-vao (gl:gen-vertex-array))
      (gl:bind-vertex-array arrowstem-vao)
      (gl:bind-buffer :array-buffer arrowstem-vbo)
      (gl:enable-vertex-attrib-array 0)
      (gl:vertex-attrib-pointer 0 2 :float nil (* 2 +float-size+) (cffi:null-pointer))
      (gl:bind-buffer :array-buffer 0)
      (gl:bind-buffer :array-buffer offset-buffer)
      (gl:enable-vertex-attrib-array 1)
      (gl:vertex-attrib-pointer 1 4 :float nil (* 4 +float-size+) (cffi:null-pointer))
      (gl:bind-buffer :array-buffer 0)
      (gl:bind-buffer :array-buffer angle-buffer)
      (gl:vertex-attrib-pointer 2 1 :float nil
                                (* 1 +float-size+) (cffi:null-pointer)) ;;; idx corresponds to position in Vertex shader code
      (gl:enable-vertex-attrib-array 2) ;;; idx corresponds to position in Vertex shader code
      (gl:bind-buffer :array-buffer 0)
      (cl-opengl-bindings:vertex-attrib-divisor 1 1)
      (cl-opengl-bindings:vertex-attrib-divisor 2 1)
      (gl:bind-vertex-array 0)

;;; for shape drawing we only need the vbo (and no vao) as we
;;; are not using indexed rendering (and only use one in?)
      
      (gl:bind-buffer :array-buffer shape-buffer)
      (gl:enable-vertex-attrib-array 0)
      (gl:vertex-attrib-pointer 0 3 :float nil (* 3 +float-size+) (cffi:null-pointer))
      (gl:bind-buffer :array-buffer 0)

      (gl:link-program shape-program)

      
      (gl:use-program circle-program)
      (cl-opengl-bindings:uniform-4f (gl:get-uniform-location circle-program "Color") 1.0 0.3 0.0 0.5)
      (gl:use-program 0)
      (gl:use-program arrow-program)
      (cl-opengl-bindings:uniform-4f (gl:get-uniform-location arrow-program "Color") 0.6 0.6 0.6 1.0)
;;;      (gl:use-program 0)
      ;; (gl:use-program shape-program)
      ;; (cl-opengl-bindings:uniform-4f (gl:get-uniform-location shape-program "Color") 0.0 1.0 1.0 1.0)
;;;      (gl:use-program 0)
      (gl:delete-shader circle-vs)
      (gl:delete-shader arrow-vs)
      (gl:delete-shader shape-vs)
      (gl:delete-shader fs)
      )))

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

(defun draw-shape (proj-mat program vao num)
          (gl:line-width 2)
          (gl:use-program program)
          (gl:uniform-matrix 
           (gl:get-uniform-location program "projection") 4 (vector proj-mat) nil)
          (gl:bind-vertex-array vao)
  (gl:draw-arrays :lines 0 num))

(defun draw-circles (proj-mat program vao num)
          (gl:line-width 2)
          (gl:use-program program)
          (gl:uniform-matrix 
           (gl:get-uniform-location program "projection") 4 (vector proj-mat) nil)
          (gl:bind-vertex-array vao)
          (gl:draw-arrays-instanced :lines 0 129 num))

(defun draw-arrows (proj-mat arrow-program arrowhead-vao arrowstem-vao num)
          (gl:use-program arrow-program)
          (gl:uniform-matrix
           (gl:get-uniform-location arrow-program "projection") 4 (vector proj-mat) nil)
          (gl:bind-vertex-array arrowhead-vao)
          (gl:draw-arrays-instanced :triangles 0 3 num)
          (gl:bind-vertex-array arrowstem-vao)
          (gl:draw-arrays-instanced :quads 0 4 num))


(defparameter *print* t)

(setf *print* t)
(setf *print* nil)

(defmethod glut:display ((w circle-window))
  (update-swank)
  (let ((curr-pos))
    ;; (gl:clear :color-buffer-bit :depth-buffer-bit)
    (with-slots (angle circle-program circle-vao arrow-program arrowhead-vao arrowstem-vao
                 offset-buffer angle-buffer
                 shape num curr-num zoom
                 x-angle y-angle z-angle) 
        w
      (continuable          
        (incf angle *angle-incr*)
        (gl-init)
        (gl:bind-buffer :array-buffer offset-buffer)
        (with-slots (freq-idx-transform-fn scale fft) shape
          (gl:bind-buffer :array-buffer offset-buffer)
          (gl:with-mapped-buffer (p1 :array-buffer :read-write)
            (loop
              for curr-offset = (complex 0.0 0.0) then next-offset
              for i below curr-num
              for n from 0
              for x = (get-idx i *mode*)
              for idx = (funcall freq-idx-transform-fn x)
              for next-offset = (* (exp (* +i+ idx angle))  (aref fft x))
                then (+ next-offset (* (exp (* +i+ idx angle)) (aref fft x)))
              do
                 (let ((offs (* i 4)))
                   (setf (cffi:mem-aref p1 :float (+ offs 0))
                         (float (realpart curr-offset) 1.0))
                   (setf (cffi:mem-aref p1 :float (+ offs 1))
                         (float (imagpart curr-offset) 1.0))
                   0.0
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
              finally (setf curr-pos next-offset)))
          (gl:bind-buffer :array-buffer 0)
          (gl:bind-buffer :array-buffer angle-buffer)
          (gl:with-mapped-buffer (p2 :array-buffer :read-write)
            (loop
              for i below curr-num
              for x = (get-idx i *mode*)
              for idx = (funcall freq-idx-transform-fn x)
              do
                 (setf (cffi:mem-aref p2 :float i) (float (+ (phase (aref fft x)) (* angle idx)) 1.0))
;;                 (setf (cffi:mem-aref p2 :float i) (float (elt point 3) 1.0))
                 (if *print* (format t " ~a~%"
                                     (cffi:mem-aref p2 :float i)))
              ))
          )
        (gl:matrix-mode :modelview)
        (gl:load-identity)
;;;      (glu:perspective 50 (/ (glut:width w) (glut:height w)) -1 1)
;;;      (gl:ortho 0 (glut:width w) 0 (glut:height w) -1 1)
;;; (gl:translate (* (- 1 1) gl-width) (* (- 1 1) gl-height) 0.0)

        (gl:rotate x-angle 1 0 0)
        (gl:rotate y-angle 0 1 0)
        (gl:rotate z-angle 0 0 1)
        (gl:scale zoom (* -1 zoom) zoom)
;;        (gl:translate (* -1 (realpart curr-pos)) (* -1 (imagpart curr-pos)) 0)
        ;;        (gl:translate 0 0 0)
        (when (circle-program w)
          (let ((proj-mat (gl:get-float :modelview-matrix)))
            (draw-circles proj-mat circle-program circle-vao curr-num)
            (draw-arrows proj-mat arrow-program arrowhead-vao arrowstem-vao curr-num))))))
  (glut:swap-buffers)
  (gl:finish))

(defmethod glut:reshape ((w circle-window) width height)
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
  (when (circle-program w)
    (let ((proj-mat (gl:get-float :modelview-matrix)))
      (gl:uniform-matrix 
       (gl:get-uniform-location (circle-program w) "projection") 
       4 (vector proj-mat) nil)))
  (when (arrow-program w)
    (let ((proj-mat (gl:get-float :modelview-matrix)))
      (gl:uniform-matrix 
       (gl:get-uniform-location (arrow-program w) "projection") 4 (vector proj-mat) nil)))
  (gl:matrix-mode :modelview)
  (gl:load-identity))

(defmethod glut:mouse ((window circle-window) button state x y)
  (continuable
    ;; (when (eql button :wheel-down)
    ;;   (set-zoom (* *zoom* 0.99)))
    ;; (when (eql button :wheel-up)
    ;;   (set-zoom (* *zoom* 1.0101)))
;;;    (format t "~a ~a ~S~%" button state (equal (list :active-ctrl) (glut:get-modifiers)))
    (case button
      (:left-button
       (when (eql state :down)))
      (:wheel-down (when (and (equal (list :active-ctrl) (glut:get-modifiers))
                              (eql state :down))
                     (setf (zoom window) (* (zoom window) 95/100))))
      (:wheel-up (when (and (equal (list :active-ctrl) (glut:get-modifiers))
                              (eql state :down))
                     (setf (zoom window) (* (zoom window) 100/95))))
      (:right-button (when (and (equal (list :active-ctrl) (glut:get-modifiers))
                              (eql state :down))
                       (setf (zoom window) 1.0))))
      ;; (when (and (eql button :right-button) (eql state :down))
    ;;   (push (make-boid-system
    ;;          `(,(float x 1.0) ,(float (* -1 y) 1.0) 0.0 0.0)
    ;;          *boids-per-click*
    ;;          window)
    ;;         (systems window))
    ;;   (format t "added boid system, ~s total~%" (length (systems window)))
    ;;   (format t " = ~:d boids~%" (reduce #'+ (systems window) :key 'boid-count)))
    ))

(defmethod glut:keyboard ((w circle-window) key x y)
  (declare (ignore x y))
  (case key
    (#\Esc (glut:destroy-current-window))))

;; Cleanup.
;; Most of the objects we created have analogous deletion function.
(defmethod glut:close ((w circle-window))
  (when (slot-boundp w 'circle-program)
   (gl:delete-program (circle-program w))))

(defun 02-indexed-circles ()
  (let ((w (make-instance 'circle-window :width 1000 :height 800)))
    (setf *window* w)
    (setf (shape w) *achtel-512*)
    (setf *curr-shape* *achtel-512*)
    (unwind-protect
         (continuable
           (glut:display-window w))
      (when (not (glut::destroyed w))
         (setf (glut::destroyed w) t)
         (glut:destroy-window (glut:id w))))))

;;; (02-indexed-circles)
;;; (gl:named-buffer-storage)

(defparameter *testvalues*
  `((0.0 0.0 1.1 0.0)
    (0.0 0.0 1.3 ,(/ pi 2))
    (0.0 0 2.0 1.5)))


#|
(progn
(02-indexed-circles)
(setf (curr-num *window*) 30))

(setf (angle *window*) 0)
(setf *print* t)
(setf *print* nil)

(progn
(setf (shape *window*) *violinschluessel-512*)
(setf *curr-shape* *violinschluessel-512*))
(setf (shape *window*) *achtel-512*)

(progn
(setf (shape *window*) *achtel-512*)
(setf *curr-shape* *achtel-512*))

(progn
(setf (shape *window*) *hessen-512*)
(setf *curr-shape* *hessen-512*))

(setf *mode* 3)
(setf *mode* 2)
(setf *mode* 3)


(setf (zoom *window*) 2)
(setf (x-angle *window*) 45)
(setf (y-angle *window*) 45)
(setf (z-angle *window*) 45)

(progn
(setf (x-angle *window*) 0)
(setf (y-angle *window*) 0)
(setf (z-angle *window*) 0)
)

(setf *angle-incr* 0.01)
(funcall (slot-value *curr-shape* 'freq-idx-transform-fn) 511)
|#
