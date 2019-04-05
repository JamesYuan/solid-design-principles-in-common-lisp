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

### Good

```scheme
(defclass printer ()
  ((data-type
    :initarg :data-type
    :reader get-data-type)))

(defmethod prints ((self printer) formatter)
  (let ((f (make-instance formatter)))
    (process f (get-data-type self))))


(defmethod process ((self epub-formatter) data-type)
  (format t "~a~%" "epub formatter's process logic goes here"))

(defclass mobi-formatter ()
  nil)

(defmethod process ((self mobi-formatter) data-type)
  (format t "~a~%" "mobi formatter's process logic goes here"))


(defparameter epub-book (make-instance 'printer :data-type "epubs"))
(defparameter mobi-book (make-instance 'printer :data-type "mobis"))

(prints epub-book 'epub-formatter) ;; epub formatter's process logic goes here
(prints mobi-book 'mobi-formatter) ;; mobi formatter's process logic goes here
```



