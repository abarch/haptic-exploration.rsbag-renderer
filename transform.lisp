#|
This file is a part of rsbag-renderer
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:rsbag-renderer)

(define-storage transform)

(defclass transform ()
  ((identifier :initarg :identifier :accessor identifier)
   (transformer :initarg :transformer :accessor transformer)
   (description :initarg :description :accessor description)
   (schema :initarg :schema :accessor schema)
   (origin :initarg :origin :accessor origin)))

(defmacro define-transform (name channel-events options &body body)
  (let ((lambda `(lambda ,channel-events
                   ,@body)))
    `(setf (transform ,(string name))
           (make-instance
            'transform
            :identifier ,(string name)
            :description ,(form-fiddle:lambda-docstring lambda)
            :transformer ,lambda
            :origin ,(or *compile-file-pathname* *load-pathname*)
            ,@options))))

(defgeneric transform-events (transform events))

(defmethod transform-events ((transform transform) events)
  (apply (transformer transform) events))

(defmethod transform-events ((identifier string) events)
  (transform-events (or (transform identifier)
                        (error "No such transform ~s" identifier))
                    events))
