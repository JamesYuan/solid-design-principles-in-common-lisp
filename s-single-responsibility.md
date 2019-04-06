# S: Single Responsibility

###### A class should have one, and  only one, reason to change.

### Bad

```scheme
(defclass truck ()
  ((brand
   :initarg :brand
   :reader get-brand)))


(defmethod send-detail ((truck truck) customer-id)
  "send truck's brand detail to customer..")

(defclass truck ()
  ((brand
   :initarg :brand
   :accessor brand)))

(defmethod get-brand ((truck truck))
  (brand truck))

(defmethod set-brand ((truck truck) new-brand)
  (setf (brand truck) new-brand))
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



