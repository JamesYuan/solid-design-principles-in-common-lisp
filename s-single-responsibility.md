# S: Single Responsibility

##### A class should have one, and  only one, reason to change.

### Bad

```lisp

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
                 
;; generated status and boot time may differ from yours
(generate-report rm1) 
;; "status number: 361. this is a status report for slow server
;; boot time estimating around 173
;; seconds from time to fully boot."
(deliver rm1)
;; send email to dummy@email.com with email content/body:
;; status number: 361. this is a status report for 
;; slow server boot time estimating around
;; 173 seconds from time to fully boot.

```

### Good

```lisp

(defclass status-report-mailer ()
  ((address
    :initarg :address
    :reader get-address)

   (report
    :initarg :report
    :reader get-report)))

(defmethod deliver ((status-report-mailer status-report-mailer))
  (format t
          "send email to ~a with email content/body: ~a~%"
          (get-address status-report-mailer)
          (get-report status-report-mailer)))

(defclass status-report-generator ()
  nil)

(defmethod generate ((status-report-generator status-report-generator))
  (concatenate 'string
               "status number: "
               (write-to-string (random 500))
               ". this is a status report for slow server boot time "
               "estimating around "
               (write-to-string (random 200))
               " seconds from time to fully boot."))

(defparameter report-data
  (make-instance 'status-report-generator))
(defparameter mailer
  (make-instance 'status-report-mailer
                 :address "dummy@email.com"
                 :report (generate report-data)))
(deliver mailer)
;; send email to dummy@email.com with email content/body:
;; status number: 91. this is a status report for
;; slow server boot time estimating around 70
;; seconds from time to fully boot.

```



