#|
This file is a part of rsbag-renderer
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:rsbag-renderer)

(defun start ()
  (start-listener 8080)
  (dolist (file (uiop:directory-files (resource "source/")))
    (when (string-equal (pathname-type file) "tide")
      (open-source :file file))))

(defun stop (&key quit)
  (stop-listener 8080)
  (dolist (source (list-sources))
    (close-source source))
  (when quit
    (uiop:quit)))
