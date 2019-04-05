# O: Open/Closed

##### Objects or entities should be open for extension, but closed for modification.

What this means is that we should write code that doesn't have to be changed every time the requirements changes. For instance, a class should be easily extendable without modifying the class itself.

Take a look at the open/closed principle violation example below.
### Bad
```scheme

(defclass circle ()
  ((radius
    :initarg :radius
    :reader get-radius)))

(defclass area-calculator ()
  ((shapes
    :initarg :shapes
    :reader get-shapes)))

(defmethod total-area ((self area-calculator))
  (reduce #'+
          (mapcar #'(lambda (x)
                      (* pi
                         (get-radius x)
                         (get-radius x)))
                  (get-shapes self))))

```

If we do want `total-area` method to calculate a sum of Rectangle areas instead of Circle, we won't be able to do that due to its specific area calculation formula ($$a = \pi * r^2$$) without modifying `total-area` method.

So how can we go over this limit?

Below code shows a better example.

### Good

```scheme

(defclass shape ()
  nil)

(defclass circle (shape)
  ((radius
    :initarg :radius
    :reader get-radius)))

(defmethod area ((self shape))
  (* pi (get-radius self) (get-radius self)))

(defclass area-calculator ()
  ((shapes
    :initarg :shapes
    :reader get-shapes)))

(defmethod total-area ((self area-calculator))
  (reduce #'+
          (mapcar #'area
                  (get-shapes self))))

```

As you've noticed, we moved the function to calculate circle area into its Circle class. This way, if we want to calculate a Rectangle shape area (or triangle, etc), we only have to create a new class with its own method to handle Rectangle area calculation.

For example, a new Rectangle class and area method which calculates a simple Rectangle shape area ($$a = w * h$$)

```scheme

(defmethod rectangle (shape)
  ((width
    :initarg :width
    :reader get-width)
   (height
    :initarg :height
    :reader get-height)))

(defmethod area ((self rectangle))
  (* (get-width self)
     (get-height self)))

```

### Full Better Example
```scheme

(defclass shape ()
  nil)

(defclass circle (shape)
  ((radius
    :initarg :radius
    :reader get-radius)))

(defmethod area ((self shape))
  (* pi (get-radius self) (get-radius self)))

(defmethod rectangle (shape)
  ((width
    :initarg :width
    :reader get-width)
   (height
    :initarg :height
    :reader get-height)))

(defmethod area ((self rectangle))
  (* (get-width self)
     (get-height self)))

(defclass area-calculator ()
  ((shapes
    :initarg :shapes
    :reader get-shapes)))

(defmethod total-area ((self area-calculator))
  (reduce #'+
          (mapcar #'area
                  (get-shapes self))))

```