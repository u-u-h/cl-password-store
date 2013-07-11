;;;; cl-password-store.asd

(asdf:defsystem #:cl-password-store
  :serial t
  :description "Describe cl-password-store here"
  :author "Utz-Uwe Haus"
  :license "LLGPL"
  :depends-on ("clsql" "ironclad")
  :components ((:file "package")
	       (:file "conditions")
               (:file "cl-password-store")))

