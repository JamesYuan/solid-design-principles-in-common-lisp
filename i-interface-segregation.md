# I: Interface Segregation

##### Clients should not be forced to depend upon interfaces that they do not use.

Because Common Lisp in particular doesn't have interface similar to static-typed language like Java or C#, and due to the Lisp's ability to do multiple inheritance, this principle carry little importance.

But, we'll try simulate this with `defgeneric` just for fun.

### So, what is all the fuss about Interface Segregation?
Basically, you don't have to implement and to depend on methods that are irrelevant for the client (eg. a class).

Let's see why this is bad, below.

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
  
;; in some other language, you are forced to implement this
;; even if it does not make sense for this parrot class.
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

;; in some other language, you are forced to implement this
;; even if it does not make sense for this penguin class.
;; since when a penguin can fly, huh? This is not a Puffin bird.
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

A Penguin can't fly. A Parrot can't (let's say) run. These classes doesn't have to depend on `b-fly` and `b-run` methods where they doesn't make sense. These extra useless code can lead to redundancy.

Let's make it better, shall we?

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



