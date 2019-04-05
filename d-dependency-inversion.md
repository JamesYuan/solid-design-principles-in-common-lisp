# D: Dependency Inversion

* ###### High level modules should not depend upon low level modules. Both should depend upon abstractions.
* ###### Abstractions should not depend upon details. Details should depend upon asbtractions.

### Bad

```scheme
(defclass printer ()
  ((data-type
    :initarg :data-type
    :reader get-data-type)))

(defmethod print-epub ((self printer))
  (let ((e (make-instance 'epub-formatter)))
    (process e (get-data-type self))))

(defmethod print-mobi ((self printer))
  (let ((m (make-instance 'mobi-formatter)))
    (process m (get-data-type self))))

(defclass epub-formatter ()
  nil)

(defmethod process ((self epub-formatter) data-type)
  "epub formatting process goes here")

(defclass mobi-formatter ()
  nil)

(defmethod process ((self mobi-formatter) data-type)
  "mobi formatting process goes here")
```

### Bad





