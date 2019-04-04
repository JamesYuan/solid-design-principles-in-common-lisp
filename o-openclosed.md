# O: Open/Closed

###### Software entities \(classes, modules, functions, etc\) should be open for extension, but closed for modification.

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