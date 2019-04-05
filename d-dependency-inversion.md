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
  (format t "~a~%data-type: ~a~%"
          "epub formatter's process logic goes here"
          data-type))

(defclass mobi-formatter ()
  nil)

(defmethod process ((self mobi-formatter) data-type)
  (format t "~a~%data-type: ~a~%"
          "mobi formatter's process logic goes here"
          data-type))

(defparameter epub-book (make-instance 'printer :data-type "epubs"))
(defparameter mobi-book (make-instance 'printer :data-type "mobis"))

(print-epub epub-book)
;; epub formatter's process logic goes here
;; data-type: epubs

(print-mobi mobi-book)
;; mobi formatter's process logic goes here
;; data-type: epubs

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

(defclass epub-formatter ()
  nil)

(defmethod process ((self epub-formatter) data-type)
  (format t "~a~%data-type: ~a~%"
          "epub formatter's process logic goes here"
          data-type))

(defclass mobi-formatter ()
  nil)

(defmethod process ((self mobi-formatter) data-type)
  (format t "~a~%data-type: ~a~%"
          "mobi formatter's process logic goes here"
          data-type))


(defparameter epub-book (make-instance 'printer :data-type "epubs"))
(defparameter mobi-book (make-instance 'printer :data-type "mobis"))

(prints epub-book 'epub-formatter) ;; epub formatter's process logic goes here
(prints mobi-book 'mobi-formatter) ;; mobi formatter's process logic goes here
```



