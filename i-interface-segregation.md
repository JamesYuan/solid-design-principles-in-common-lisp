# I: Interface Segregation

##### Clients should not be forced to depend upon interfaces that they do not use.

Because Common Lisp in particular doesn't have interface like in a static-typed language like Java or C#, and due to the Lisp's ability to do multiple inheritance, this principle carry little importance.
### Bad

```lisp
(defclass bird ()
  nil)

(defgeneric b-eat (bird))
(defgeneric b-sleep (bird))
(defgeneric b-fly (bird))
(defgeneric b-run (bird))

(defclass parrot (bird)
  nil)

(defmethod b-eat ((parrot parrot))
  (format t "~a~%" "the parrot eats"))

(defmethod b-sleep ((parrot parrot))
  (format t "~a~%" "the parrot sleeps"))

(defmethod b-fly ((parrot parrot))
  (format t "~a~%" "the parrot flies"))
(defmethod b-run ((parrot parrot))
  (format t
          "~a~%"
          "this is wrong! a parrot cannot really run! :("))

(defclass penguin (bird)
  nil)

(defmethod b-eats ((penguin penguin))
  (format t "~a~%" "the penguin eats"))

(defmethod b-sleep ((penguin penguin))
  (format t "~a~%" "the penguin sleeps"))

(defmethod b-run ((penguin penguin))
  (format t "~a~%" "the penguin runs"))

(defmethod b-fly ((penguin penguin))
  (format t "~a~%" "this is wrong. a penguin cannot fly! :("))


(defparameter clawy (make-instance 'parrot))
(defparameter pingu (make-instance 'penguin))

(b-eat clawy) ;; the parrot eats
(b-sleep clawy) ;; the parrot sleeps
(b-run clawy) ;; this is wrong!. a parrot cannot really run! :(
(b-fly clawy) ;; the parrot flies

(b-eat pingu) ;; the penguin eats
(b-sleep pingu) ;; the penguin sleeps
(b-run pingu) ;; the penguin runs
(b-fly pingu) ;; this is wrong. a penguin cannot fly! :(
```

### Good

```lisp
(defclass bird ()
  nil)

(defgeneric b-eat (bird))
(defgeneric b-sleep (bird))

(defclass flightless-bird (bird)
  nil)

(defgeneric b-run (flightless-bird))

(defclass flying-bird (bird)
  nil)

(defgeneric b-fly (flying-bird))

(defclass parrot (flying-bird)
  nil)

(defmethod b-eat ((parrot parrot))
  (format t "~a~%" "the parrot eats"))

(defmethod b-sleep ((parrot parrot))
  (format t "~a~%" "the parrot sleeps"))

(defmethod b-fly ((parrot parrot))
  (format t "~a~%" "the parrot flies"))

(defclass penguin (flightless-bird)
  nil)

(defmethod b-eat ((penguin penguin))
  (format t "~a~%" "the penguin eats"))

(defmethod b-sleep ((penguin penguin))
  (format t "~a~%" "the penguin sleeps"))

(defmethod b-run ((penguin penguin))
  (format t "~a~%" "the penguin runs"))

(defparameter clawy (make-instance 'parrot))
(defparameter pingu (make-instance 'penguin))

(b-eat clawy) ;; the parrot eats
(b-sleep clawy) ;; the parrot sleeps
(b-fly clawy) ;; the parrot flies

(b-eat pingu) ;; the penguin eats
(b-sleep pingu) ;; the penguin sleeps
(b-run pingu) ;; the penguin runs
```



