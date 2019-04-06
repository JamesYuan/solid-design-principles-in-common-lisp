# S: Single Responsibility

###### A class should have one, and  only one, reason to change.

### Bad

```scheme

(in-package :cl-user)
(defpackage reserve
  (:use :cl))
(in-package :reserve)

(defclass status-report-mailer ()
  ((address
    :initarg :address
    :reader get-address)

   (report
    :initarg :report
    :initform ""
    :reader get-report
    :accessor report)))

(defmethod deliver ((status-report-mailer status-report-mailer))
  (format t
          "send email to ~a with email content/body: ~a~%"
          (get-address status-report-mailer)
          (get-report status-report-mailer)))

(defmethod generate-report ((status-report-mailer status-report-mailer))
  (let ((r (concatenate 'string
                        "status number: "
                        (write-to-string (random 500))
                        ". this is a status report for slow server boot time "
                        "estimating around "
                        (write-to-string (random 200))
                        " seconds from time to fully boot.")))
    (setf (report status-report-mailer) r)))

(defparameter rm1
  (make-instance 'status-report-mailer
                 :address "dummy@email.com"))
(generate-report rm1) 
;; "status number: 361. this is a status report for slow server
;; boot time estimating around 173
;; seconds from time to fully boot."
(deliver rm1)

```

### Good

```scheme
(defclass detail-sender ()
  ((customer-id
    :initarg :customer-id
    :accessor customer-id)))

(defmethod get-customer-id ((detail-sender detail-sender))
  (customer-id detail-sender))

(defmethod set-customer-id ((detail-sender detail-sender) new-customer-id)
    (setf (customer-id detail-sender) new-customer-id))

(defmethod send-detail ((detail-sender detail-sender))
  (send (customer-id detail-sender)))
```



