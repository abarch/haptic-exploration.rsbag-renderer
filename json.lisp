
#|
This file is a part of rsbag-renderer
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:rsbag-renderer)

(defvar *json-output*)
(defvar *started*)
(defparameter *string-replaces*
  (let ((map (make-hash-table :test 'eql)))
    (loop for (k v) in '((#\\ "\\\\")
                         (#\" "\\\"")
                         (#\Backspace "\\b")
                         (#\Page "\\f")
                         (#\Newline "\\n")
                         (#\Return "\\r")
                         (#\Tab "\\t"))
          do (setf (gethash k map) v))
    map))

(declaim (inline json-delimiter
                 json-null
                 json-false
                 json-true)
         (type stream *json-output*)
         (type boolean *started*))

(defun json-delimiter (&optional (dest *json-output*))
  (declare (optimize speed) (type stream dest))
  (if *started*
      (write-byte (char-code #\,) dest)
      (setf *started* T)))

(defun json-null (&optional (dest *json-output*))
  (declare (optimize speed) (type stream dest))
  (write-string "null" dest))

(defun json-false (&optional (dest *json-output*))
  (declare (optimize speed) (type stream dest))
  (write-sequence #.(sb-ext:string-to-octets "false") dest))

(defun json-true (&optional (dest *json-output*))
  (declare (optimize speed) (type stream dest))
  (write-sequence #.(sb-ext:string-to-octets "true") dest))

(declaim (inline write-json))
(defun write-json (object &optional (dest *json-output*))
  (declare (optimize speed)
	   (type stream dest)
	   (notinline write-json))
  (typecase object
    (integer
     (princ object dest))
    
    (ratio
     (write-json (coerce object 'double-float) dest))
    
    (float
     (let ((*read-default-float-format* 'double-float))
       (format dest "~F" (coerce object 'double-float))))
    
    (string
     (write-byte (char-code #\") dest)
     (loop with start = 0 while start
	  with rep = nil
	  for end = (position-if (lambda (c) (setf rep (gethash c *string-replaces*)))
				 object :start start)
	do
	  (write-sequence (sb-ext:string-to-octets object :start start :end end) dest)
	  (when end
	    (write-string (string rep) dest)
	    (incf end))
	  (setf start end))
     (write-byte (char-code #\") dest))
    
    (list
     (with-json-array (dest)
       (loop for i in object
             do (entry i))))
    
    (vector
     (with-json-array (dest)
       (loop for i across object
             do (entry i))))
    
    (T (write-json (princ-to-string object) dest)))
  object)

(define-compiler-macro write-json (&whole whole object &optional (dest *json-output*)
				   &environment env)
  (if (constantp object env)
      (let ((value (eval object)))
	(typecase value
	  (string
	   (if (position-if (lambda (c) (gethash c *string-replaces*)) value)
	       whole
	       `(write-sequence ,(sb-ext:string-to-octets (format nil "\"~A\"" value)) ,dest)))
	  (t
	   whole)))
      whole))

(defmacro with-delimiters ((destination left right) &body body)
  `(progn
     (let ((*started* NIL))
       (write-char ,left ,destination)
       ,@body)
     (write-char ,right ,destination)))

(defmacro with-json-array ((&optional (destination '*json-output*)) &body body)
  `(with-delimiters (,destination #\[ #\])
     (flet ((entry (value)
              (with-json-entry (,destination)
                (write-json value ,destination))
              NIL))
       (declare (ignorable #'entry)
		(inline entry)
		(dynamic-extent #'entry))
       ,@body)))

(defmacro with-json-entry ((&optional (destination '*json-output*)) &body body)
  `(progn (json-delimiter ,destination)
          ,@body))

(defmacro with-json-object ((&optional (destination '*json-output*)) &body body)
  `(with-delimiters (,destination #\{ #\})
     (flet ((pair (key value)
              (with-json-value (key ,destination)
                (write-json value ,destination))
              NIL))
       (declare (ignorable #'pair)
		(inline pair)
		(dynamic-extent #'pair))
       ,@body)))

(defmacro with-json-value ((key &optional (destination '*json-output*)) &body body)
  `(progn
     (json-delimiter ,destination)
     (write-json ,key ,destination)
     (write-byte ,(char-code #\:) ,destination)
     ,@body))

(defmacro with-json-output ((&optional stream) &body body)
  (if stream
      `(let ((*json-output* ,stream))
         ,@body)
      `(with-output-to-string (*json-output*)
         ,@body)))
