# L: Interface Segregation

###### Clients should not be forced to depend upon interfaces that they do not use.

### Bad

```scheme
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
  (format t "~a~%" "this is wrong. penguin cannot fly! :("))


(defparameter clawy (make-instance 'parrot))
(defparameter pingu (make-instance 'penguin))

(b-run clawy)
(b-sleep clawy)
(b-run clawy)
(b-fly clawy)

(b-run pingu)
(b-sleep pingu)
(b-run pingu)
(b-fly pingu)

```

### Good

```scheme

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
  (format t "~a~%" "this is wrong. penguin cannot fly! :("))


(defparameter clawy (make-instance 'parrot))
(defparameter pingu (make-instance 'penguin))

(b-run clawy)
(b-sleep clawy)
(b-run clawy)
(b-fly clawy)

(b-run pingu)
(b-sleep pingu)
(b-run pingu)
(b-fly pingu)



```