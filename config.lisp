#|
This file is a part of rsbag-renderer
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:rsbag-renderer)

(defun start ()
  (start-listener 8080)
  (dolist (file (uiop:command-line-arguments))
    (open-source :file file))
  (dolist (file (uiop:directory-files (root-pathname "visualizers/") "*.js"))
    (load-visualizer file)))

(defun stop (&key quit)
  (stop-listener 8080)
  (dolist (source (list-sources))
    (close-source source))
  (when quit
    (uiop:quit)))

(defun main (&rest args)
  (declare (ignore args))
  (start)
  (dolist (arg (uiop:command-line-arguments))
    (let ((file (uiop:parse-native-namestring arg)))
      (when (string-equal (pathname-type file) "tide")
        (open-source :file file))))
  (sleep most-positive-fixnum))
