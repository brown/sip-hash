(in-package #:common-lisp)

(defpackage #:sip-hash
 (:documentation "An implementation of the SipHash family of hash functions.")
 (:use #:common-lisp)
 (:import-from #:com.google.base
               #:octet-vector
               #:uint64
               #:vector-index)
 (:import-from #:nibbles
               #:ub16ref/le
               #:ub32ref/le
               #:ub64ref/le)
 (:export #:hash-64-2-4
          #:hash-64-4-8
          #:hash-128-2-4
          #:hash-128-4-8))
