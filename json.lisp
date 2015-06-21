#|
This file is a part of rsbag-renderer
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:rsbag-renderer)

(defvar *json-output*)
(defvar *started*)

(declaim (inline json-delimiter
                 json-null
                 json-false
                 json-true))

(defun json-delimiter (delim &optional (dest *json-output*))
  (if *started*
      (write-char (char delim 0) dest)
      (setf *started* T)))

(defun json-null (&optional (dest *json-output*))
  (write-string "null" dest))

(defun json-false (&optional (dest *json-output*))
  (write-string "false" dest))

(defun json-true (&optional (dest *json-output*))
  (write-string "true" dest))

(defun write-json (object &optional (dest *json-output*))
  (typecase object
    (string (prin1 object dest))
    (list (format NIL "[~{~s~^,~}]" object))
    (T (princ object dest))))

(defmacro with-delimiters ((destination left right) &body body)
  `(unwind-protect
        (let ((*started* NIL))
          (write-string ,left ,destination)
          ,@body)
     (write-string ,right ,destination)))

(defmacro with-json-array ((&optional (destination '*json-output*)) &body body)
  `(with-delimiters (,destination "[" "]")
     (macrolet ((with-json-entry (&body body)
                  `(progn (json-delimiter ",")
                          ,@body)))
       (flet ((entry (value)
                (with-json-entry ()
                  (write-json value ,destination))))
         ,@body))))

(defmacro with-json-object ((&optional (destination '*json-output*)) &body body)
  `(with-delimiters (,destination "{" "}")
     (macrolet ((with-json-key (&body body)
                  `(progn (json-delimiter ",")
                          (with-json-string (,',destination)
                            ,@body)))
                (with-json-value (&body body)
                  `(progn (json-delimiter ":")
                          ,@body)))
       (labels ((key (string)
                  (with-json-key
                      (write-string string ,destination)))
                (value (value)
                  (with-json-value
                      (write-json value ,destination)))
                (pair (key value)
                  (key key) (value value)))
         ,@body))))

(defmacro with-json-string ((&optional (destination '*json-output*)) &body body)
  `(with-delimiters (,destination "\"" "\"")
     ,@body))

(defmacro with-json-output (() &body body)
  `(with-output-to-string (*json-output*)
     ,@body))
