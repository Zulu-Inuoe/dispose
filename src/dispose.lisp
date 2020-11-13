(defpackage #:com.inuoe.dispose
  (:use #:cl)
  (:import-from #:alexandria)
  (:export
   #:protect-progn
   #:dispose
   #:define-dispose
   #:using
   #:disposable
   #:disposedp))

(in-package #:com.inuoe.dispose)

(define-method-combination protect-progn
    (&optional (order :most-specific-first))
  ((around (:around))
   (primary (protect-progn) :order order :required t))
  (let ((primary-form (labels ((recurse (m)
                                 (destructuring-bind (method . rest) m
                                   (if rest
                                       `(unwind-protect (call-method ,method)
                                          ,(recurse rest))
                                       `(call-method ,method)))))
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

(defmacro define-dispose (obj &body body)
  "Macro for defining a dispose form.
Light wrapper around `defmethod' with the correct method combination.
  `obj' can be either a symbol, denoting the variable & class, or a list (var class)"
  (multiple-value-bind (obj-name obj-class)
      (etypecase obj
        (symbol
         (values obj obj))
        (list
         (destructuring-bind (name class) obj
           (values name class))))
    `(defmethod dispose protect-progn ((,obj-name ,obj-class))
       ,@body)))

(define-dispose (obj stream)
  "`close' the stream."
  (close obj))

(defmacro using ((binding &rest rest-bindings) &body body)
  "Perform sequentially the bindings in `bindings' in a protected form.
`dispose' will be called in opposite order on each binding on exit."
  (multiple-value-bind (body decl) (alexandria:parse-body body)
    (let* ((bindings (cons binding rest-bindings))
           (bindings (if (and (= 2 (length bindings))
                              (symbolp (car bindings)))
                         (list bindings)
                         bindings)))
      (labels ((recurse (bindings)
                 (cond
                   ((null (cdr bindings))
                    (destructuring-bind (var val) (car bindings)
                      `(let ((,var ,val))
                         ,@decl
                         (unwind-protect ,(if (null (cdr body)) (car body) `(progn ,@body))
                           (dispose ,var)))))
                   (t
                    (destructuring-bind (var val) (car bindings)
                      `(let ((,var ,val))
                         (unwind-protect ,(recurse (cdr bindings))
                           (dispose ,var))))))))
        (recurse bindings)))))

(defclass disposable ()
  ((%disposed
    :type boolean
    :initform nil
    :reader disposedp
    :documentation "Boolean indicating whether the object has been disposed."))
  (:documentation "Mixin that prevents being disposed multiple times."))

(defmethod dispose :around ((obj disposable))
  "Calls `dispose' on `obj' if it has not already been disposed."
  (unless (disposedp obj)
    (setf (slot-value obj '%disposed) t)
    (call-next-method))
  (values))

(define-dispose (obj disposable)
  (declare (ignore obj))
  (values))
