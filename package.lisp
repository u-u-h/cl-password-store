;;;; package.lisp

(defpackage #:cl-password-store
  (:use #:cl)
  ;; classes:
  (:export #:user-token-mixin
	   #:password-store)
  ;; store creation and database
  (:export #:open-password-database
	   #:open-password-store
	   #:*default-password-database*
	   #:*default-password-hash*
	   #:*default-password-store*
	   #:*default-password-store-pepper*)
  ;; interface of user-token-mixin
  (:export #:user-token-id)
  ;; interface of password-store
  (:export 
   ;; creation
   #:register-user
   #:get-user-confirmation-token
   #:confirm-registration
   ;; query
   #:user-knownp
   #:authenticate-user
   #:all-users
   ;; password reset
   #:get-password-reset-token
   #:reset-password
   ;; deletion
   #:delete-user
   ;; Conditions
   #:user-exists #:user-unknown
   #:password-token-expired #:confirmation-token-expired
   ))

