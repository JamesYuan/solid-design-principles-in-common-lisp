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



