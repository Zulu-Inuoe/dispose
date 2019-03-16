(defpackage #:dispose
  (:use
   #:cl)
  (:export
   #:protect-progn
   #:dispose
   #:define-dispose
   #:with-disposables*
   #:disposable
   #:disposedp

   #:finalizer
   #:define-finalizer
   #:finalizable))
