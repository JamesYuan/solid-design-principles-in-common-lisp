# S: Single Responsibility

##### A class should have one, and  only one, reason to change.
The idea is to make a class only handle a single relevant responsibility (though, this is up to the programmer to decide).

Martin suggests that we define each responsibility of a class as a reason for change.  If you can think of more than one motivation for changing a class, it probably has more than one responsibility.

See below for a SRP violation example

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

;; this is okay. mailer should deliver a report.
(defmethod deliver ((status-report-mailer status-report-mailer))
  (format t
          "send email to ~a with email content/body: ~a~%"
          (get-address status-report-mailer)
          (get-report status-report-mailer)))

;; this is incorrect; mailer class should not be responsible
;; for generating a report
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

As you can see above, `status-report-mailer` class is handling both distinct functionalities. Report generation and report delivery. This will force you to modify `status-report-mailer` class if you wish to set up a new value or generate different kind of report template, which it has nothing to do with.

Let's fix this by moving `generate-report` method into its own class.

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



