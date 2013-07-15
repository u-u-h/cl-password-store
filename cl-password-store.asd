;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; -*-
;;;
;;; cl-password-store.asd --- Password management for Common Lisp (web) applications.

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
(defpackage cl-password-store-system
  (:use #:cl #:asdf))

(in-package :cl-password-store-system)

(defsystem #:cl-password-store
  :serial t
  :description "Password management for Common Lisp (web) applications."
  :author "Utz-Uwe Haus <lisp@uuhaus.de>"
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

