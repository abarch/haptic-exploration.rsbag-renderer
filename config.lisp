#|
This file is a part of rsbag-renderer
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:rsbag-renderer)

(defun start ()
  (start-listener 8080)
  )

(defun stop ()
  (stop-listener 8080)
  (dolist (source (list-sources))
    (close-source source)))
