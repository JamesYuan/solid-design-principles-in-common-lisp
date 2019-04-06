# D: Dependency Inversion

* ##### High level modules should not depend upon low level modules. Both should depend upon abstractions.
* ##### Abstractions should not depend upon details. Details should depend upon asbtractions.

Dependency Inversion Principle encourages us to create higher level modules with its complex logic in such a way to be reusable and unaffected by any change from the lower level modules in our application. To achieve this kind of behavior in our apps, we introduce abstraction which decouples higher from lower level modules.

### Wait! What are these *low-level* and *high-level* modules again?

### Bad

```lisp
(defclass printer ()
  ((data-type
    :initarg :data-type
    :reader get-data-type)))

(defmethod print-epub ((printer printer))
  (let ((e (make-instance 'epub-formatter)))
    (process e (get-data-type printer))))

(defmethod print-mobi ((printer printer))
  (let ((m (make-instance 'mobi-formatter)))
    (process m (get-data-type printer))))

(defclass epub-formatter ()
  nil)

(defmethod process ((epub-formatter epub-formatter) data-type)
  (format t "~a~%data-type: ~a~%"
          "epub formatter's process logic goes here"
          data-type))

(defclass mobi-formatter ()
  nil)

(defmethod process ((mobi-formatter mobi-formatter) data-type)
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
;; data-type: mobis
```

### Good

```lisp
(defclass printer ()
  ((data-type
    :initarg :data-type
    :reader get-data-type)))

(defmethod prints ((printer printer) formatter)
  (let ((f (make-instance formatter)))
    (process f (get-data-type printer))))

(defclass epub-formatter ()
  nil)

(defmethod process ((epub-formatter epub-formatter) data-type)
  (format t "~a~%data-type: ~a~%"
          "epub formatter's process logic goes here"
          data-type))

(defclass mobi-formatter ()
  nil)

(defmethod process ((mobi-formatter mobi-formatter) data-type)
  (format t "~a~%data-type: ~a~%"
          "mobi formatter's process logic goes here"
          data-type))


(defparameter epub-book (make-instance 'printer :data-type "epubs"))
(defparameter mobi-book (make-instance 'printer :data-type "mobis"))

(prints epub-book 'epub-formatter)
;; epub formatter's process logic goes here
;; data-type: epubs

(prints mobi-book 'mobi-formatter)
;; mobi formatter's process logic goes here
;; data-type: mobis
```



