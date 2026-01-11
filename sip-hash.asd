(defsystem sip-hash
  :author "Robert Brown <robert.brown@gmail.com>"
  :bug-tracker "https://github.com/brown/sip-hash/issues"
  :description "SipHash hash functions"
  :homepage "https://github.com/brown/sip-hash"
  :license "Public domain"
  :long-description
"SipHash, a cryptographically strong family of hash functions designed by
Jean-Philippe Aumasson and Daniel J. Bernstein.  (http://131002.net/siphash/)"
  :name "SipHash"
  :source-control (:git "https://github.com/brown/sip-hash.git")
  :version "1.8"
  :defsystem-depends-on (com.google.base)
  :depends-on (com.google.base
               nibbles
               #+sbcl sb-rotate-byte)
  :in-order-to ((test-op (test-op sip-hash/test)))
  :components
  ((:file "package")
   (:fast-unsafe-source-file "sip-hash" :depends-on ("package"))))

(defsystem sip-hash/test
  :author "Robert Brown <robert.brown@gmail.com>"
  :bug-tracker "https://github.com/brown/sip-hash/issues"
  :description "Test code for package SIP-HASH."
  :homepage "https://github.com/brown/sip-hash"
  :license "Public domain"
  :name "SipHash test"
  :source-control (:git "https://github.com/brown/sip-hash.git")
  :version "1.8"
  :depends-on (sip-hash hu.dwim.stefil)
  :components
  ((:file "sip-hash-test")))

(defmethod perform ((operation test-op) (component (eql (find-system 'sip-hash/test))))
  (symbol-call 'sip-hash-test 'test-sip-hash))
