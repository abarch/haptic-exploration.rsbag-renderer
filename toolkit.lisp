#|
This file is a part of rsbag-renderer
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:rsbag-renderer)

(defvar *debugger* NIL)

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

(defun resource (path &optional error-p)
  (or (probe-file (merge-pathnames path))
      (probe-file (asdf:system-relative-pathname :rsbag-renderer path))
      (and error-p (error "Could not find resource ~s" path))))

(defun template (path &optional (error-p T))
  (resource (merge-pathnames path "template/") error-p))

(defun static-file (path &optional (error-p T))
  (resource (merge-pathnames path "static/") error-p))

(defun assoc-all (item alist)
  (loop for (name . val) in alist
        when (string-equal name item)
        collect val))

(defun maybe-unlist (list)
  (if (cdr list) list (car list)))

(defun dissect-error (err)
  (v:error :server "Unhandled error:~&~a" (dissect:present err NIL))
  (when *debugger* (invoke-debugger err)))
