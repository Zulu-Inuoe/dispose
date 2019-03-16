(in-package #:dispose)

(defgeneric finalizer (object)
  (:documentation
   "Create a finalizer for the object.
A finalizer is a function of no arguments that cleans up any resources used by 'object'.
The finalizer should not hold a reference to `object', and may run as part of the garbage collector.
See `trivial-garbage:finalize'.")
  (:method-combination list))

(defmacro define-finalizer (class (&rest slots) &body body)
  "Defines a `finalizer' method for `class' in an which the slot-values values of `slots' are bound to their respective names.
Ex:

  (define-finalizer net-logger (sock log-stream)
    (usocket:socket-close sock)
    (close log-stream))"
  (let ((object-sym (gensym "OBJECT")))
    `(defmethod finalizer list ((,object-sym ,class))
       (let (,@(mapcar
                 (lambda (slot-name)
                   `(,slot-name (slot-value ,object-sym ',slot-name)))
                 slots))
         (lambda () ,@body)))))
