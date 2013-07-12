;;;; cl-password-store.asd
(defpackage cl-password-store-system
  (:use #:cl #:asdf))

(in-package :cl-password-store-system)

(defsystem #:cl-password-store
  :serial t
  :description "A library providing password handling for web applications."
  :author "Utz-Uwe Haus"
  :license "LLGPL"
  :version "0.1.0"
  :depends-on ("clsql" "ironclad")
  :components ((:file "package")
	       (:file "conditions")
               (:file "cl-password-store"))
  :in-order-to ((test-op (load-op cl-password-store-test)))
  :perform (test-op :after (op c)
                    (funcall (intern "RUN!" :cl-password-store-tests)
                             (intern "TESTS" :cl-password-store-tests))))
(defmethod operation-done-p
    ((op test-op) (c (eql (find-system :cl-password-store))))
  (values nil))

(defsystem cl-password-store-test
  :depends-on ("cl-password-store" "fiveam")
  :components ((:file "tests")))

