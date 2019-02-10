(in-package #:dispose)

(defmethod dispose protect-progn ((obj stream))
  (close obj))
