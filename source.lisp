#|
This file is a part of rsbag-renderer
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:rsbag-renderer)

(define-storage source)

(defun list-sources ()
  (sort (loop for id being the hash-keys of *source* collect id) #'string<))

(defclass source ()
  ((identifier :initarg :identifier :accessor identifier))
  (:default-initargs
   :identifier (error "IDENTIFIER required.")))

(defclass channel ()
  ((identifier :initarg :identifier :accessor identifier)
   (source :initarg :source :accessor channel-source))
  (:default-initargs
   :identifier (error "IDENTIFIER required.")
   :channel-source (error "SOURCE required.")))

(defclass event ()
  ((id :initarg :id :accessor id)
   (channel :initarg :channel :accessor event-channel)
   (timestamp :initarg :timestamp :accessor timestamp)
   (payload :initarg :payload :accessor payload))
  (:default-initargs
   :id (error "ID required.")
   :channel (error "CHANNEL required.")
   :timestamp (error "TIMESTAMP required.")
   :payload (error "PAYLOAD required.")))

(defgeneric source-type (source))

(defgeneric open-source (type identifier &key &allow-other-keys)
  (:method :around (type identifier &key)
    (let ((source (call-next-method)))
      (setf (source (identifier source))
            source))))

(defgeneric close-source (source)
  (:method :after ((source source))
    (remove-source (identifier source))))

(defgeneric channel (source identifier))

(defgeneric channels (source))

(defgeneric channel-length (channel))

(defgeneric event (channel index))


(defclass file-source (source)
  ((bag :initarg :bag :accessor bag)))

(defmethod source-type ((source file-source))
  :file)

(defmethod open-source ((type (eql :file)) pathname &key)
  (let* ((pathname (uiop:enough-pathname (etypecase pathname
                                             (pathname pathname)
                                             (string (pathname pathname)))
                                           *default-pathname-defaults*)))
    (make-instance 'file-source
                   :identifier (namestring pathname)
                   :bag (rsbag:open-bag pathname :direction :input))))

(defmethod close-source ((source file-source))
  (close (bag source))
  source)
