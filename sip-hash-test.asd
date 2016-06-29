
(defsystem sip-hash-test
  :name "SipHash test"
  :description "Test code for package SIP-HASH."
  :version "1.6"
  :author "Robert Brown"
  :license "Public domain."
  :depends-on (sip-hash hu.dwim.stefil)
  :components
  ((:file "sip-hash_test")))

(defmethod perform ((operation test-op) (component (eql (find-system 'sip-hash-test))))
  (funcall (read-from-string "sip-hash-test:test-sip-hash")))
