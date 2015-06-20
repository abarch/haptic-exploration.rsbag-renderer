#|
This file is a part of rsbag-renderer
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem :rsbag-renderer
  :defsystem-depends-on (:radiance)
  :class "radiance:module"
  :version "0.0.1"
  :license "LGPL2.1"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "RSBag Data Web Server"
  :serial T
  :components ((:file "package")
               (:file "front")
               (:file "api")
               (:file "config"))
  :depends-on (:radiance
               :i-hunchentoot
               :i-verbose
               :r-clip)
  :build-operation asdf:program-op
  :build-pathname "rsbag-renderer"
  :entry-point "rsbag-renderer:start")
