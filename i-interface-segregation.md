# L: Interface Segregation

###### Clients should not be forced to depend upon interfaces that they do not use.

### Bad

```scheme
(defclass bird ()
  nil)

(defgeneric b-eat (bird))
(defgeneric b-sleep (bird))
(defgeneric b-fly (bird))

(defclass parrot (bird)
  nil)

(defmethod b-eat ((parrot parrot))
  (format t "~a~%" "the parrot eats"))

(defmethod b-sleep ((parrot parrot))
  (format t "~a~%" "the parrot sleeps"))

(defmethod b-fly ((parrot parrot))
  (format t "~a~%" "the parrot flies"))

(defclass penguin (bird)
  nil)

(defmethod b-eats ((penguin penguin))
  (format t "~a~%" "the penguin eats"))

(defmethod b-sleep ((penguin penguin))
  (format t "~a~%" "the penguin sleeps"))

(defmethod b-fly ((penguin penguin))
  (format t "~a~%" "this is wrong. penguin cannot fly! :("))
```

### Good

```scheme
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



(b-eat clawy)

(b-sleep clawy)

(b-fly clawy)



(b-eat pingu)

(b-sleep pingu)

(b-run pingu)

```