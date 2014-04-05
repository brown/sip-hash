
;;;; sip-hash.asd

(defsystem sip-hash
  :name "SipHash"
  :description "SipHash hash functions"
  :long-description
"SipHash, a cryptographically strong family of hash functions designed by
Jean-Philippe Aumasson and Daniel J. Bernstein.  (http://131002.net/siphash/)"
  :version "1.5"
  :author "Robert Brown"
  :license "Public domain."
  :defsystem-depends-on (com.google.base)
  :depends-on (com.google.base
               nibbles
               #+sbcl sb-rotate-byte)
  :in-order-to ((test-op (test-op sip-hash-test)))
  :components
  ((:file "package")
   (:fast-unsafe-source-file "sip-hash" :depends-on ("package"))))
