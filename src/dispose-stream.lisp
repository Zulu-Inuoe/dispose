(in-package #:dispose)

(define-dispose (obj stream)
  (close obj))
