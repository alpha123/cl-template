(asdf:defsystem #:cl-template
  :description "A simple output-agnostic templating system for Common Lisp."
  :version "0.0.1"
  :author "Peter Cannici <turkchess123@gmail.com>"
  :license "MIT"
  :in-order-to ((test-op (test-op #:cl-template-tests)))
  :components ((:file "packages")
               (:file "util" :depends-on ("packages"))
               (:file "cl-template" :depends-on ("packages" "util"))
               (:file "predefined" :depends-on ("packages"))))

(asdf:defsystem #:cl-template-tests
  :description "Unit tests for cl-template."
  :version "0.0.1"
  :author "Peter Cannici <turkchess123@gmail.com>"
  :license "MIT"
  :depends-on (#:cl-template #:fiveam)
  :components ((:file "packages")
               (:file "test/util" :depends-on ("packages"))
               (:file "test/cl-template" :depends-on ("packages"))))
