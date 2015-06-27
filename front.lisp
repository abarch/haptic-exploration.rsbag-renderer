#|
This file is a part of rsbag-renderer
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:rsbag-renderer)

(define-page front "/" ()
  (redirect "/select"))

(define-page select "/select" ()
  (clip:process (template "select.ctml")
                :sources (list-sources)
                :transforms (list-transforms)))

(define-page render "/render" ()
  (clip:process (template "render.ctml")))

(define-page admin "/admin" ()
  (clip:process (template "admin.ctml")))
