
;;;; package.lisp

(in-package #:common-lisp-user)

(defpackage #:sip-hash
 (:documentation "An implementation of the sip-hash hash function.")
 (:use #:common-lisp #:com.google.base)
 (:export #:hash-2-4
          #:hash-4-8))
