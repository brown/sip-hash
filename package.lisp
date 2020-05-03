(in-package #:common-lisp)

(defpackage #:sip-hash
 (:documentation "An implementation of the SipHash family of hash functions.")
 (:use #:common-lisp #:com.google.base)
 (:export #:hash-64-2-4
          #:hash-64-4-8
          #:hash-128-2-4
          #:hash-128-4-8))
