
;;;; sip-hash.asd

(in-package #:common-lisp-user)

(defpackage #:sip-hash-system
 (:documentation "System definition for the SIP-HASH package.")
 (:use #:common-lisp #:asdf))

(in-package #:sip-hash-system)

(defsystem sip-hash
 :name "SipHash"
 :description "SipHash hash functions"
 :long-description
"SipHash, a cryptographically strong family of hash functions designed by
Jean-Philippe Aumasson and Daniel J. Bernstein.  (http://131002.net/siphash/)"
 :version "1.0"
 :author "Robert Brown"
 :license "Public domain."
 :depends-on (com.google.base #+sbcl sb-rotate-byte)
 :in-order-to ((test-op (test-op sip-hash-test)))
 :components
 ((:file "package")
  (:file "sip-hash" :depends-on ("package"))))
