# S: Single Responsibility

###### A class should have one, and  only one, reason to change.



### Bad

```scheme
(defclass printer ()
  ((document-type
    :initarg :document-type
    :accessor document-type)))

(defmethod process-email ((self printer))
  "process email..")

(defmethod send-email ((self printer))
  "send document as email")

(defvar printer-one (make-instance 'printer :document-type "docx"))
(process-email printer-one)
(send-email printer-one)
```





