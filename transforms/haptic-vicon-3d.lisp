#|
This file is a part of rsbag-renderer
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:rsbag-renderer)

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
  (destructuring-bind (r g b) rgb
    (let ((rgb 0))
      (setf (ldb (byte 255 0) rgb) b)
      (setf (ldb (byte 255 8) rgb) g)
      (setf (ldb (byte 255 16) rgb) r)
      rgb)))

(defun hue-rgb (h)
  (let ((x (round (* (- 1 (abs (- (mod (/ h 60) 2) 1))) 255))))
    (cond ((<=   0 h  60) (list 255 x 0))
          ((<=  60 h 120) (list x 255 0))
          ((<= 120 h 180) (list 0 255 x))
          ((<= 180 h 240) (list 0 x 255))
          ((<= 240 h 300) (list x 0 255))
          ((<= 300 h 360) (list 255 0 x)))))

(defun intensity-hue (intensity)
  (let ((mini 0) (maxi 4095)
        (minh 0) (maxh 360))
    (+ (* (/ (- intensity mini) (- maxi mini))
          (- maxh minh))
       minh)))

(define-transform haptic-vicon-3d (vicon haptic) ()
  "Basic specific transform tailored to haptic and vicon combined recordings towards a GL3DVisualizer."
  (let* ((vicon (payload vicon))
         (haptic (payload haptic))
         (points (rst.devices.mocap:vicon-points vicon))
         (channels (rst.devices.haptic:haptic-channels haptic)))
    (with-json-object ()
      (loop for point across points
            do (let* ((coords (rst.devices.mocap:vicon/marker-point-position point))
                      (name (rst.devices.mocap:vicon/marker-point-name point))
                      (channel (vicon-haptic-channel name)))
                 (key name)
                 (with-json-value ()
                   (with-json-object () (pair "X" (rst.math:vec3ddouble-x coords))
                     (pair "Y" (rst.math:vec3ddouble-y coords))
                     (pair "Z" (rst.math:vec3ddouble-z coords))
                     (when channel
                       (pair "color" (rgb-hex (hue-rgb (intensity-hue (aref channels channel)))))))))))))
