#|
This file is a part of rsbag-renderer
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:rsbag-renderer)

(defvar *template-dir* (asdf:system-relative-pathname :rsbag-renderer "template/"))

(defun template (name)
  (merge-pathnames name *template-dir*))

(defun process (template &rest args)
  (plump:serialize
   (apply #'clip:process (plump:parse (template template)) args)))

(hunchentoot:define-easy-handler (front :uri "/") ()
  (process "select.ctml"))

(hunchentoot:define-easy-handler (admin :uri "/admin") ()
  (process "admin.ctml"))

(hunchentoot:define-easy-handler (render :uri "/render") ()
  (process "render.ctml"))
