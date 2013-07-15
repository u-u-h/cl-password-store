;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; -*-
;;;
;;; package.lisp --- Password management for Common Lisp (web) applications.

;; Copyright (C) 2013 Utz-Uwe Haus <lisp@uuhaus.de>
;;
;; This code is free software; you can redistribute it and/or modify
;; it under the terms of the version 2 of the GNU Library General
;; Public License as published by the Free Software Foundation, as
;; clarified by the prequel found in LICENSE.LLGPL
;;

;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, 
;; Boston, MA  02110-1301  USA
;;
;; Commentary:
;; 

;;;
;;; A sample session of typical uses
;;;
(in-package :cl-user)
(setq cl-password-store:*default-password-database*
      (cl-password-store:open-password-database))
;; => clsql database object
(setq cl-password-store:*default-password-store*
      (cl-password-store:open-password-store))
;; => a password store object

(cl-password-store:user-knownp "Bob")
;; => NIL
(cl-password-store:authenticate-user "Bob" "foo")
;; => condition: user-unknown
(cl-password-store:register-user "Bob" :needs-confirmation-within nil
				 :password "foo")
;; => "Bob"
(cl-password-store:all-users)
;; => ("Bob")

(cl-password-store:delete-user "Bobby")
;; => condition: user-unknown
(cl-password-store:delete-user "Bobby" :no-exist-ok T)
;; => NIL
(cl-password-store:delete-user "Bob")
;; => "Bob"
(cl-password-store:all-users)
;; => NIL

(cl-password-store:register-user "Bob" :needs-confirmation-within nil
				 :password "foo")
;; => "Bob"
(cl-password-store:authenticate-user "Bob" "fooness")
;; => NIL
(cl-password-store:authenticate-user "Bob" "foo")

;;
(cl-password-store:register-user "Alice" 
				 :needs-confirmation-within 
				 (clsql:make-duration) ;; i.e. 0 sec.
				 :password "foo")
;; "Alice" , ...long numeric string...
(cl-password-store:authenticate-user "Alice" "foo")
;; => NIL 
(multiple-value-bind (name confirmation-token)
    (cl-password-store:register-user "Frank" 
				     :needs-confirmation-within 
				     (clsql:make-duration) ;; i.e. 0 sec.
				     :password "foo")
  (cl-password-store:confirm-registration name "42")
  ;; => NIL
  (cl-password-store:confirm-registration name confirmation-token)
  ;; => condition: confirmation-token-expired
  )

(multiple-value-bind (name confirmation-token)
    (cl-password-store:register-user "Frank" 
				     :needs-confirmation-within 
				     (clsql:make-duration) ;; i.e. 0 sec.
				     :password "foo")
  (cl-password-store:confirm-registration name "42")
  ;; => NIL
  (cl-password-store:confirm-registration name confirmation-token)
  ;; => condition: confirmation-token-expired
  )
(let ((new-token
       (cl-password-store:get-user-confirmation-token 
	"Frank" :validity-duration (clsql:make-duration :day 1))))
  (cl-password-store:confirm-registration "Frank" new-token))
;; => T

