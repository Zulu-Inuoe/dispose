(in-package #:dispose)

(defclass disposable ()
  ((%disposed
    :type boolean
    :initform nil
    :reader disposedp
    :documentation "Boolean indicating whether the object has been disposed."))
  (:documentation "Mixin that prevents being disposed multiple times."))

(defmethod dispose protect-progn :around ((obj disposable))
  "Calls `dispose' on `obj' if it has not already been disposed."
  (unless (disposedp obj)
    (setf (slot-value obj '%disposed) t)
    (call-next-method))
  (values))
