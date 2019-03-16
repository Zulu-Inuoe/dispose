(in-package #:dispose)

(defun %compose-finalizers (finalizers)
  (cond
    ((null finalizers) nil)
    ((null (cdr finalizers)) (car finalizers))
    (t (lambda () (mapc #'funcall finalizers)))))

(defclass finalizable (disposable)
  ((%finalizer
    :type (or null function)
    :reader %finalizer))
  (:documentation
   "Mixin to establish reasonable dispose and finalize methods.
`finalizable' objects establish a finalizer for themselves via 'trivial-garbage:finalize', utilizing 'finalizer'.
A 'dispose' implementation is provided that invokes 'finalizer', and cancels finalization via 'trivial-garbage:cancel-finalization'."))

(defmethod initialize-instance :around ((object finalizable) &key &allow-other-keys)
  "Registers the object for finalization"
  (unwind-protect
       (call-next-method)
    (setf (slot-value object '%finalizer) (%compose-finalizers (finalizer object)))
    (when (%finalizer object)
      (trivial-garbage:finalize object (%finalizer object)))))

(define-dispose (object finalizable)
  "Cancels finalization on 'obj' and invokes its finalizer."
  (when (%finalizer object)
    (trivial-garbage:cancel-finalization object)
    (funcall (%finalizer object)))
  (values))
