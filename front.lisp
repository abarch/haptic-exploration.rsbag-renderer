#|
This file is a part of rsbag-renderer
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:rsbag-renderer)

(define-page front "/" ()
  (redirect "/select"))

(defun process (temp &rest args)
  (let ((*package* (load-time-value (find-package '#:rsbag-renderer))))
    (apply #'clip:process (template temp) args)))

(define-page select "/select" ()
  (process "select.ctml"
           :sources (list-sources)
           :transforms (list-transforms)
           :visualizers (list-visualizers)))

(define-page render "/render" ()
  (process "render.ctml"
           :source (source (post/get "source"))))

(define-page admin "/admin" ()
  (process "admin.ctml"
           :sources (list-sources)
           :transforms (list-transforms)
           :visualizers (list-visualizers)))
