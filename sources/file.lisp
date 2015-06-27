#|
This file is a part of rsbag-renderer
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:rsbag-renderer)

(defclass file-source (source rsbag:bag)
  ((identifier :initarg :identifier :accessor identifier)))

(defclass file-channel (channel rsbag:channel)
  ())

(defclass file-event ()
  ((event :initarg :event :accessor event-event)
   (channel :initarg :channel :accessor event-channel)))

(defmethod source-type ((source file-source))
  :file)

(defmethod open-source ((type (eql :file)) pathname &key)
  (let* ((pathname (uiop:enough-pathname (etypecase pathname
                                           (pathname pathname)
                                           (string (pathname pathname)))
                                         *default-pathname-defaults*))
         (bag (rsbag:open-bag pathname :direction :input))
         (source (change-class bag 'file-source :identifier pathname)))
    (loop for channel in (rsbag:bag-channels source)
          do (change-class channel 'file-channel))
    source))

(defmethod close-source ((source file-source))
  (close source)
  source)

(defmethod channel ((source file-source) identifier)
  (change-class (rsbag:bag-channel source identifier) 'file-channel))

(defmethod channels ((source file-source))
  (mapcar (lambda (c) (change-class c 'file-channel))
          (rsbag:bag-channels source)))

(defmethod channel-length ((channel file-channel))
  (length channel))

(defmethod channel-source ((channel file-channel))
  (rsbag:channel-bag channel))

(defmethod event ((channel file-channel) index)
  (make-instance
   'file-event
   :event (rsbag:entry channel index)
   :channel channel))

(defmethod identifier ((channel file-channel))
  (rsbag:channel-name channel))

(defmethod id ((event file-event))
  (rsb:event-sequence-number (event-event event)))

(defmethod timestamp ((event file-event))
  (getf (rsb:event-timestamps (event-event event)) :create))

(defun name-schema (name)
  (let ((pos (position #\: name)))
    (if pos (subseq name (1+ pos)) name)))

(defmethod payload ((event file-event))
  (rsb.converter:wire->domain
   :protocol-buffer
   (rsb:event-data (event-event event))
   (find-symbol (name-schema (identifier (event-channel event))) :keyword)))