# S: Single Responsibility

###### A class should have one, and  only one, reason to change.

### Bad

```scheme

(defclass truck ()
  ((brand
   :initarg :brand
   :accessor brand)))

(defmethod get-brand ((self truck))
  (brand self))

(defmethod set-brand ((self truck) new-brand)
  (setf (brand self) new-brand))

(defmethod send-detail ((self truck) customer-id)
  "send truck's brand detail to customer..")

```

### Good
```scheme
(defclass truck ()
  ((brand
   :initarg :brand
   :accessor brand)))

(defmethod get-brand ((self truck))
  (brand self))

(defmethod set-brand ((self truck) new-brand)
  (setf (brand self) new-brand))

(defclass detail-sender ()
  ((customer-id
    :initarg :customer-id
    :accessor customer-id)))

(defmethod get-customer-id ((self detail-sender))
  (customer-id self))

(defmethod set-customer-id ((self detail-sender) new-customer-id)
  (setf (customer-id self) new-brand))

(defmethod send-detail ((self detail-sender))
  (send (get-customer-id)))
```



