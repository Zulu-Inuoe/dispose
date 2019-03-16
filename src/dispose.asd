(defsystem #:dispose
  :version "0.0.1"
  :description "Common 'dispose' mechanism."
  :author "Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>"
  :license "CC0 1.0 Universal"
  :components
  ((:file "package")
   (:file "dispose" :depends-on ("package"))
   (:file "disposable" :depends-on ("package" "dispose"))
   (:file "dispose-stream" :depends-on ("package" "dispose"))
   (:file "finalizer" :depends-on ("package"))
   (:file "finalizable" :depends-on ("package" "finalizer" "dispose" "disposable")))
  :depends-on
  (#:trivial-garbage))
