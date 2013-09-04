
;;;; sip-hash-test.asd

(defsystem sip-hash-test
 :depends-on (sip-hash hu.dwim.stefil)
 :components
 ((:file "sip-hash_test")))

(defmethod perform ((operation test-op) (component (eql (find-system 'sip-hash-test))))
 (funcall (read-from-string "sip-hash-test:test-sip-hash")))
