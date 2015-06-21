#|
This file is a part of rsbag-renderer
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:rsbag-renderer)

;; Page dispatch
(define-storage page)

(defmacro define-page (name uri () &body body)
  `(setf (page ',name)
         (list ,(format NIL "^~a$" uri) (lambda () ,@body))))

(defun dispatch (to)
  (v:debug :server "Dispatching to ~s" to)
  (loop for page being the hash-values of *page*
        for (path func) = page
        do (when (cl-ppcre:scan path to)
             (return (funcall func)))))

;; Api dispatch
(defmacro define-api (name args () &body body))

;; Statics
(define-page static "/static/(.*)" ()
  ;; Insecure, don't care.
  (static-file (subseq
                (hunchentoot:url-decode
                 (hunchentoot:script-name hunchentoot:*request*)
                 :utf-8)
                (length "/static/"))
               NIL))

;; Minor
(defun redirect (to)
  (hunchentoot:redirect to))

(defun resource (path &optional error-p)
  (or (probe-file (merge-pathnames path))
      (probe-file (asdf:system-relative-pathname :rsbag-renderer path))
      (and error-p (error "Could not find resource ~s" path))))

(defun template (path &optional (error-p T))
  (resource (merge-pathnames path "template/") error-p))

(defun static-file (path &optional (error-p T))
  (resource (merge-pathnames path "static/") error-p))

;; Server backend
(define-storage listener)

(defun start-listener (port)
  (when (listener port)
    (error "Listener on port ~d already exists." port))
  (let ((listener (make-instance 'hunchentoot:easy-acceptor
                                 :port port
                                 :access-log-destination NIL
                                 :message-log-destination NIL)))
    (setf (listener port) listener)
    (hunchentoot:start listener)
    (v:info :server "Started listener on http://localhost:~d/" port))
  port)

(defun stop-listener (port)
  (unless (listener port)
    (error "No listener on port ~d active." port))
  (hunchentoot:stop (listener port))
  (remove-listener port)
  (v:info :server "Stopped listener on http://localhost:~d/" port)
  port)

(defun post-handler (result)
  (etypecase result
    (plump:node
     (setf (hunchentoot:content-type*) "application/xhtml+xml; charset=utf-8")
     (plump:serialize result NIL))
    (pathname (hunchentoot:handle-static-file result))
    (string result)
    ((array (unsigned-byte 8)) result)
    (null (setf (hunchentoot:return-code*) 404)
     "No result (404)")))

(progn
  (defun pre-handler (request)
    (let* ((path (hunchentoot:url-decode
                  (hunchentoot:script-name request)
                  :utf-8))
           (result (ignore-errors
                    (handler-bind ((error (lambda (err)
                                            (v:error :server "Unhandled error: ~&~a" (dissect:present err NIL)))))
                      (dispatch path)))))
      (lambda ()
        (post-handler result))))
  (setf hunchentoot:*dispatch-table* (list #'pre-handler)))
