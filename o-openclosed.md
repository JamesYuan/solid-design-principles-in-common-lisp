# O: Open/Closed

###### Software entities \(classes, modules, functions, etc\) should be open for extension, but closed for modification.

### Bad
```scheme

(defclass rectangle ()
  ((width
    :initarg :width
    :accessor width)
   (height
    :initarg :height
    :accessor height)))

(defclass area-calculator ()
  ((shape
    :initarg :shape
    :accessor shape)))

(defmethod total-area ((self area-calculator))
  (* (width (shape self))
     (height (shape self))))

```