#|
This file is a part of rsbag-renderer
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:rsbag-renderer)

(setf radiance:*config-path* (asdf:system-relative-pathname :rsbag-renderer "radiance.uc.lisp"))
(radiance:remove-uri-dispatcher 'welcome)

(defun start ()
  (radiance:startup))
