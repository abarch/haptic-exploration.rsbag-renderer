#|
This file is a part of rsbag-renderer
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:rsbag-renderer)

(defmacro define-storage (basename)
  (let ((var (intern (format NIL "*~a*" (string-upcase basename))))
        (fun (intern (format NIL "~a" (string-upcase basename))))
        (rem (intern (format NIL "REMOVE-~a" (string-upcase basename)))))
    `(progn
       (defvar ,var (make-hash-table :test 'equal))

       (defun ,fun (id)
         (gethash id ,var))

       (defun (setf ,fun) (val id)
         (setf (gethash id ,var) val))

       (defun ,rem (id)
         (remhash id ,var))
       
       (values ',fun ',rem ',var))))
