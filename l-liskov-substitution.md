# L: Liskov Substitution

###### Let _Φ\(x\) be a property provable about objects x of type T_. Then _Φ\(y\) should be true for objects y of type S where S is a subtype of T._

##### in other words: Likov's Substitution Principle states that if a program module is using a Base class, then the reference to the Base class can be replaced with a Derived class without affecting the functionality of the program module.

```lisp


(defclass rectangle ()
  ((width
    :initarg :width
    :initform 0
    :reader get-width
    :accessor width)

   (height
    :initarg :height
    :initform 0
    :reader get-height
    :accessor height)))

(defmethod area ((rectangle rectangle))
  (* (get-width rectangle)
     (get-height rectangle)))

(defclass square (rectangle)
  nil)

(defmethod set-width ((square square) w)
  (setf (width square) w)
  (setf (height square) w))

(defmethod set-height ((square square) h)
  (setf (height square) h)
  (setf (width square) h))

(defparameter square-area (make-instance 'square))

(set-width square-area 5)
(set-height square-area 10)
(area square-area) ;; 100 instead of 50

```