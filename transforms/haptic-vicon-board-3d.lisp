#|
This file is a part of rsbag-renderer
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:rsbag-renderer)
(defpackage #:rsbag-renderer.transform.haptic-vicon-board-3d
  (:use #:cl #:alexandria #:rsbag-renderer)
  (:export #:haptic-vicon-board-3d))
(in-package #:rsbag-renderer.transform.haptic-vicon-board-3d)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (rsbag-helper:load-proto-file (asdf:system-relative-pathname :rsbag-renderer "proto/Vicon.proto"))
  (rsbag-helper:load-proto-file (asdf:system-relative-pathname :rsbag-renderer "proto/Haptic.proto"))
  (rsbag-helper:load-proto-file (asdf:system-relative-pathname :rsbag-renderer "proto/InteractiveBoard.proto")))

(defparameter *vicon-haptic-map*
  (let ((map (make-hash-table :test 'equalp)))
    (loop for (k v) in '(("subject-4-baby-1" 7)
                         ("subject-4-baby-2" 8)
                         ("subject-4-ring-1" 35)
                         ("subject-4-ring-2" 33)
                         ("subject-4-middle-1" 40)
                         ("subject-4-middle-2" 45)
                         ("subject-4-index-1" 18)
                         ("subject-4-index-2" 30)
                         ("subject-4-thumb-1" 23)
                         ("subject-4-thumb-2" 22))
          do (setf (gethash k map) v))
    map))

(defun vicon-haptic-channel (name)
  (gethash name *vicon-haptic-map*))

(defun rgb-hex (rgb)
  (declare (optimize speed))
  (destructuring-bind (r g b) rgb
    (declare (type (integer 0 255) r g b))
    (let ((rgb 0))
      (setf (ldb (byte 255 0) rgb) b)
      (setf (ldb (byte 255 8) rgb) g)
      (setf (ldb (byte 255 16) rgb) r)
      rgb)))

(defun hue-rgb (h)
  (declare (optimize speed)
           (type (integer 0 360) h))
  (let ((x (round (* (- 1 (abs (- (mod (/ h 60) 2) 1))) 255))))
    (cond ((<=   0 h  60) (list 255 x 0))
          ((<=  60 h 120) (list x 255 0))
          ((<= 120 h 180) (list 0 255 x))
          ((<= 180 h 240) (list 0 x 255))
          ((<= 240 h 300) (list x 0 255))
          ((<= 300 h 360) (list 255 0 x)))))

(defun intensity-hue (intensity)
  (declare (optimize speed)
           (type (integer 0 4095) intensity))
  (let ((mini 0) (maxi 4095)
        (minh 0) (maxh 360))
    (round
     (+ (* (/ (- intensity mini) (- maxi mini))
           (- maxh minh))
        minh))))

(defun find-type (type &rest args)
  (loop for arg in args
        when (typep arg type)
        return arg))

;; (defun apply-to-vecotr(quat, vec)
;;   (flet ((apply (quat, vec)
;; 	   (let 
;; 	   ))
;;     (apply vec))
;; )

(define-transform haptic-vicon-board-3d (a b &optional c) ()
  "Basic specific transform tailored to haptic and vicon combined recordings towards a GL3DVisualizer."

  (let* ((a (payload a))
         (b (payload b))
	 (c (when c (payload c)))

         (vicon (find-type 'rst.devices.mocap:vicon a b c))         
	 (points (rst.devices.mocap:vicon-points vicon))

	 #+unused (haptic (find-type 'rst.devices.haptic:haptic a b c))
;	 (channels (rst.devices.haptic:haptic-channels haptic))

	 (board (find-type 'rst.devices.interactiveboard:interactive-board a b c))
	 
	 (boardconfig (rst.devices.interactiveboard:interactive-board-boardconfig board))
	 (pose (rst.devices.interactiveboard:board-configuration-pose boardconfig))
	 (translation (rst.geometry:pose-translation pose))
	 (board-x (rst.geometry:translation-x translation))
	 (board-y (rst.geometry:translation-y translation))
	 (board-z (rst.geometry:translation-z translation))
	 (quaternion (rst.geometry:pose-rotation pose))
	 (bricks (rst.devices.interactiveboard:interactive-board-bricks board))
	 )
    (declare (type double-float board-x board-y board-z))
    (with-json-object ()
      (loop for brick across bricks 
	   do 
	   (let* ((row (rst.devices.interactiveboard:interactive-board/brick-row brick))
		  (col (rst.devices.interactiveboard:interactive-board/brick-col brick))
		  (covered   (rst.devices.interactiveboard:interactive-board/brick-covered brick))
		  (rho       (rst.devices.interactiveboard:interactive-board/brick-rho brick))
		  (base-color (if (evenp (+ row (mod col 2))) #x00ff00 #x0000ff))
		  
		  (rho-color (floor (lerp (/ (clamp rho 0 6000) 6000) #xA0 #x00)))
		  (color (logior rho-color (ash rho-color 8) (ash rho-color 16) base-color))
		  #+covered (color (logior (if covered #x000000 #x808080) base-color))
		  
		  (color-string (format nil "#~6,'0x" color))
		  (name (format nil "~D-~D" row col)))
	     (declare (type (integer 0 100000) rho))
	   (with-json-value (name)
	     (with-json-object ()
	       (pair "X" (+ board-x (* 0.03 col)))
	       (pair "Y" (+ board-y (* 0.03 row)))
	       (pair "Z" (- board-z 0.04 (lerp (/ (clamp rho 0 6000) 6000) 0 .02))) ; TODO find real maximum instead of 6000
	       (pair "size" 0.028)
	       (pair "color" color-string)))))

      (loop for point across points
            do (let* ((coords (rst.devices.mocap:vicon/marker-point-position point))
                      (name (rst.devices.mocap:vicon/marker-point-name point))
                      (channel (vicon-haptic-channel name)))
                 (with-json-value (name)
                   (with-json-object ()
                     (pair "X" (the double-float (rst.math:vec3ddouble-x coords)))
                     (pair "Y" (the double-float (rst.math:vec3ddouble-y coords)))
                     (pair "Z" (the double-float (rst.math:vec3ddouble-z coords)))
                     (when channel
                       (pair "color" (rgb-hex (hue-rgb (intensity-hue 0 #+no (aref channels channel)))))))))))))
