#|
This file is a part of rsbag-renderer
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:rsbag-renderer)

(define-page front #@"/" ()
  (redirect "/select"))

(define-page select (#@"/select" 100) (:lquery (template "select.ctml"))
  (r-clip:process T))

(define-page render #@"/render" (:lquery (template "render.ctml"))
  (r-clip:process T))

(define-page admin #@"/admin" (:lquery (template "admin.ctml"))
  (r-clip:process T))
