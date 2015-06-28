#|
This file is a part of rsbag-renderer
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:rsbag-renderer)

(define-api shutdown () ()
  (stop :quit T))

(define-api error (&optional message) ()
  (error (or message "An utterly surprising error.")))

(defun safely-parse-date (string)
  (local-time:parse-timestring
   string :allow-missing-elements T
          :allow-missing-date-part T
          :allow-missing-time-part NIL
          :allow-missing-timezone-part T))

(define-api source/list () ()
  (with-api-output ("Source listing")
    (with-json-array ()
      (dolist (source (list-sources))
        (entry (identifier source))))))

(define-api source/channel/list (source) ()
  (let ((source (or (source source)
                    (error "No such source ~s" source))))
    (with-api-output ("Channel listing")
      (with-json-array ()
        (dolist (channel (channels source))
          (entry (identifier channel)))))))

(define-api source/channel/event (source channel &optional (transform "direct") (skip "0") amount from to) ()
  (let* ((source (or (source source)
                     (error "No such source ~s" source)))
         (channels (loop for chan in (ensure-list channel)
                         collect (or (channel source chan)
                                     (error "No such channel ~s" chan))))
         (transform (or (transform transform)
                        (error "No such transform ~s" transform)))
         (skip (parse-integer skip))
         (amount (and amount (parse-integer amount)))
         (from (and from (safely-parse-date from)))
         (to (and to (safely-parse-date to))))
    (assert (<= 0 skip) () "SKIP must be 0 or greater.")
    (assert (or (null amount) (<= 0 amount)) () "AMOUNT must be 0 or greater.")
    (assert (or (null from) (null to) (local-time:timestamp<= from to)) () "TO cannot be a time before FROM.")
    (with-api-output ("Event delivery")
      (with-json-array ()
        (loop with in-range = (not from)
              for i from skip
              for events = (mapcar (<< #'event i) channels)
              while (and (or (not amount) (< (- i skip) amount))
                         (or (not to) (loop for ev in events always (local-time:timestamp<= ev to))))
              do (cond (in-range
                        (with-json-entry ()
                          (with-json-object ()
                            (pair "id" i)
                            (pair "timestamp" (local-time:timestamp-to-unix (timestamp (first events))))
                            (with-json-value ("data")
                              (transform-events transform events)))))
                       ;; Not the best way to handle this. Ideally, stepping should
                       ;; happen on a per-channel basis or something similar.
                       ((loop for ev in events always (local-time:timestamp<= from ev))
                        (setf in-range T))))))))
