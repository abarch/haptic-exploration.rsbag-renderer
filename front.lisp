#|
This file is a part of rsbag-renderer
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:rsbag-renderer)

(setf radiance:*config-path* (asdf:system-relative-pathname :rsbag-renderer "radiance.uc.lisp"))
(radiance:remove-uri-dispatcher 'welcome)

(define-page front #@"/" ()
  (redirect "/select"))

(define-page select #@"/select" ()
  (r-clip:process (template "select.ctml")))

(define-page render #@"/render" ()
  (r-clip:process (template "render.ctml")))

(define-page admin #@"/admin" ()
  (r-clip:process (template "admin.ctml")))
