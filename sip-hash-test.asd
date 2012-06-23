
;;;; sip-hash-test.asd

(in-package #:common-lisp-user)

(defpackage #:sip-hash-test-system
 (:documentation "System definition for testing the SIP-HASH package.")
 (:use #:common-lisp #:asdf))

(in-package #:sip-hash-test-system)

(defsystem sip-hash-test
 :depends-on (sip-hash hu.dwim.stefil)
 :components
 ((:file "sip-hash_test")))

(defmethod perform ((operation test-op) (component (eql (find-system 'sip-hash-test))))
 (funcall (read-from-string "sip-hash-test:test-sip-hash")))
