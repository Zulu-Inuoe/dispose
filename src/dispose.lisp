(in-package #:dispose)

(define-method-combination protect-progn
    (&optional (order :most-specific-first))
  ((around (:around))
   (primary (protect-progn) :order order :required t))
  (let ((primary-form (labels ((recurse (m)
                                 (cond
                                   ((rest m)
                                    `(unwind-protect (call-method ,(first m))
                                       ,(recurse (rest m))))
                                   (t
                                    `(call-method ,(first m))))))
                        `(progn
                           ,(recurse primary)
                           (values)))))
    (if around
        `(progn
           (call-method ,(first around) (,@(rest around) (make-method ,primary-form)))
           (values))
        primary-form)))

(defgeneric dispose (object)
  (:documentation
   "Interface for disposing an object. This cleans up any resources used by said object.
No return value.")
  (:method-combination protect-progn))

(defmacro define-dispose ((obj-name obj-class) &body body)
  "Macro for defining a dispose form.
Light wrapper around `defmethod' with the correct method combination."
  `(defmethod dispose protect-progn ((,obj-name ,obj-class))
     ,@body))

(defmacro with-disposables* ((&rest bindings) &body body)
  "Perform sequentially the bindings in `bindings' in a protected form.
`dispose' will be called in opposite order on each binding on exit."
  (labels ((recurse (bindings)
             (cond
               (bindings
                (destructuring-bind (var val)
                    (car bindings)
                  `(let ((,var ,val))
                     (unwind-protect
                          ,(recurse (cdr bindings))
                       (dispose ,var)))))
               (t
                `(locally ,@body)))))
    (recurse bindings)))
