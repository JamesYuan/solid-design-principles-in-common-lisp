# O: Open/Closed

###### Software entities \(classes, modules, functions, etc\) should be open for extension, but closed for modification.

```scheme
(defclass meta ()
  ((init
    :initarg :init
    :accessor init)))
```