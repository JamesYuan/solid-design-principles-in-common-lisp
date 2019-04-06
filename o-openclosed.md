# O: Open/Closed

##### Objects or entities should be open for extension, but closed for modification.

What this means is that we should write code that doesn't have to be changed every time the requirements changes. For instance, a class should be easily extendable without modifying the class itself.

Take a look at the open/closed principle violation example below.

### Bad

```lisp
(defclass circle ()
  ((radius
    :initarg :radius
    :reader get-radius)))

(defclass area-calculator ()
  ((shapes
    :initarg :shapes
    :reader get-shapes)))

(defmethod total-area ((area-calculator area-calculator))
  (reduce #'+
          (mapcar #'(lambda (x)
                      (* pi
                         (get-radius x)
                         (get-radius x)))
                  (get-shapes area-calculator))))

(defparameter *circle-one*
  (make-instance 'area-calculator
                 :shapes
                 (list (make-instance 'circle :radius 5)
                       (make-instance 'circle :radius 6)
                       (make-instance 'circle :radius 2))))

(total-area *circle-one*) ;; 204.20352248333654d0
```

If we do want `total-area` method to calculate a sum of Rectangle areas instead of Circle, we won't be able to do that due to its specific area calculation formula \(a = pi \* r^2\) for a circle area without modifying `total-area` method.

So how can we go over this limit?

Below code shows a better example.

### Good

```lisp

(defclass circle ()
  ((radius
    :initarg :radius
    :reader get-radius)))

(defmethod area ((circle circle))
  (* pi (get-radius circle) (get-radius circle)))

(defclass area-calculator ()
  ((shapes
    :initarg :shapes
    :reader get-shapes)))

(defmethod total-area ((area-calculator area-calculator))
  (reduce #'+
          (mapcar #'area
                  (get-shapes area-calculator))))

(defparameter *circle-one*
  (make-instance 'area-calculator
                 :shapes
                 (list (make-instance 'circle :radius 5)
                       (make-instance 'circle :radius 6)
                       (make-instance 'circle :radius 2))))

(total-area *circle-one*) ;; 204.20352248333654d0
```

As you've noticed, we moved the function to calculate circle area into its Circle class. This way, if we want to calculate a Rectangle shape area \(or triangle, etc\), we only have to create a new class with its own method to handle Rectangle area calculation.

For example, a new Rectangle class and area method which calculates a simple Rectangle shape area \(a = w \* h\)

```lisp
(defclass rectangle ()
  ((width
    :initarg :width
    :reader get-width)

   (height
    :initarg :height
    :reader get-height)))

(defmethod area ((rectangle rectangle))
  (* (get-width rectangle)
     (get-height rectangle)))
```

### Full Better Example

```lisp

(defclass circle ()
  ((radius
    :initarg :radius
    :reader get-radius)))

(defmethod area ((circle circle))
  (* pi (get-radius circle) (get-radius circle)))

(defclass rectangle ()
  ((width
    :initarg :width
    :reader get-width)

   (height
    :initarg :height
    :reader get-height)))

(defmethod area ((rectangle rectangle))
  (* (get-width rectangle)
     (get-height rectangle)))

(defclass area-calculator ()
  ((shapes
    :initarg :shapes
    :reader get-shapes)))

(defmethod total-area ((area-calculator area-calculator))
  (reduce #'+
          (mapcar #'area
                  (get-shapes area-calculator))))

(defparameter *circle-one*
  (make-instance 'area-calculator
                 :shapes
                 (list (make-instance 'circle :radius 5)
                       (make-instance 'circle :radius 3)
                       (make-instance 'circle :radius 12))))

(defparameter *rectangle-one*
  (make-instance 'area-calculator
                 :shapes
                 (list (make-instance 'rectangle :height 5 :width 10)
                       (make-instance 'rectangle :height 9 :width 20)
                       (make-instance 'rectangle :height 23 :width 44))))

(total-area *circle-one*) ;; 559.2034923389832d0
(total-area *rectangle-one*) ;; 1242
```



