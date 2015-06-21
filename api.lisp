#|
This file is a part of rsbag-renderer
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:rsbag-renderer)

(define-api shutdown () ()
  (stop :quit T))

(define-api coffee () ()
  (with-json-output ()
    (write-json "Hi!")))
