(in-package #:cl-password-store)

(define-condition cl-password-store-condition ()
  ())

(define-condition user-exists (cl-password-store-condition)
  ((user-token :accessor get-user-token :initarg :user-token))
  (:report
   (lambda (condition stream)
     (format stream "User ~A already exists." (get-user-token condition)))))

(define-condition user-unknown (cl-password-store-condition)
  ((user-token :accessor get-user-token :initarg :user-token))
  (:report
   (lambda (condition stream)
     (format stream "User ~A does not exist." (get-user-token condition)))))

(define-condition password-token-expired (cl-password-store-condition)
  ((user-token :accessor get-user-token :initarg :user-token)
   (expiry     :accessor get-expiry     :initarg :expiry))
  (:report
   (lambda (condition stream)
     (format stream "User ~A's password reset token expired ~A."
	     (get-user-token condition)
	     (clsql:format-time nil (get-expiry condition))))))

(define-condition confirmation-token-expired (cl-password-store-condition)
  ((user-token :accessor get-user-token :initarg :user-token)
   (expiry     :accessor get-expiry     :initarg :expiry))
    (:report
   (lambda (condition stream)
     (format stream "User ~A's confirmation token expired ~A."
	     (get-user-token condition)
	     (clsql:format-time nil (get-expiry condition))))))





