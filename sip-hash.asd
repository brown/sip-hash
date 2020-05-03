(defsystem sip-hash
  :name "SipHash"
  :description "SipHash hash functions"
  :long-description
"SipHash, a cryptographically strong family of hash functions designed by
Jean-Philippe Aumasson and Daniel J. Bernstein.  (http://131002.net/siphash/)"
  :version "1.7"
  :author "Robert Brown <robert.brown@gmail.com>"
  :license "Public domain"
  :defsystem-depends-on (com.google.base)
  :depends-on (com.google.base
               nibbles
               #+sbcl sb-rotate-byte)
  :in-order-to ((test-op (test-op sip-hash/test)))
  :components
  ((:file "package")
   (:fast-unsafe-source-file "sip-hash" :depends-on ("package"))))

(defsystem sip-hash/test
  :name "SipHash test"
  :description "Test code for package SIP-HASH."
  :version "1.7"
  :author "Robert Brown <robert.brown@gmail.com>"
  :license "Public domain"
  :depends-on (sip-hash hu.dwim.stefil)
  :components
  ((:file "sip-hash-test")))

(defmethod perform ((operation test-op) (component (eql (find-system 'sip-hash/test))))
  (symbol-call 'sip-hash-test 'test-sip-hash))
