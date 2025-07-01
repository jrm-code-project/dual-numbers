(defsystem "dual-numbers"
  :description "A library for dual numbers in Common Lisp"
  :author "Joe Marshall"
  :license "MIT"
  :version "0.9.0"
  :depends-on ("alexandria"
               "generic-arithmetic")
  :components ((:file "dual-numbers" :depends-on ("package"))
               (:file "dual-numbers-tests" :depends-on ("dual-numbers" "package"))
               (:file "package")))
