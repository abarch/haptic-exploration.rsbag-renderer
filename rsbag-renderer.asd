#|
This file is a part of rsbag-renderer
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem :rsbag-renderer
  :version "0.0.1"
  :license "LGPL2.1"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "RSBag Data Web Server"
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "json")
               (:file "server")
               (:file "source")
               (:file "transform")
               (:file "front")
               (:file "api")
               (:file "config")
               (:module "sources"
                :components ((:file "file")))
               (:module "transforms"
                :components ((:file "haptic-vicon-3d"))))
  :depends-on (:hunchentoot
               :cl-ppcre
               :dissect
               :verbose
               :clip
               :cl-rsbag
               :cl-rsb-common
               :form-fiddle
               :rsbag-tidelog
               :rsb-converter-protocol-buffer)
  :build-operation asdf:program-op
  :build-pathname "rsbag-renderer"
  :entry-point "rsbag-renderer:main")
