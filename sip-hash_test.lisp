;;;; SipHash unit tests.
;;;; Derived fron Dmitry Chestnykh's public domain Go code.

;;;; Author: Robert Brown (robert.brown@gmail.com)

(in-package #:common-lisp-user)

(defpackage #:sip-hash-test
 (:documentation "Test code in the SIP-HASH package.")
 (:use #:common-lisp
       #:com.google.base
       #:hu.dwim.stefil
       #:sip-hash)
 (:export #:test-sip-hash))

(in-package #:sip-hash-test)
(declaim #.*optimize-default*)

(defsuite (test-sip-hash :in root-suite) ()
 (run-child-tests))

(in-suite test-sip-hash)

(defconst +golden+
 `((#x0706050403020100
    #x0f0e0d0c0b0a0908
    ,(make-octet-vector 15 :initial-contents
      '(#x00 #x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08 #x09 #x0a #x0b #x0c #x0d #x0e))
    #xa129ca6149be45e5)
   (0 0 ,(string-to-utf8-octets "Hello world") #xc9e8a3021f3822d9)
   (0 0 ,(string-to-utf8-octets "") #x1e924b9d737700d7)
   (0 0 ,(string-to-utf8-octets "12345678123") #xf95d77ccdb0649f)
   (0 0 ,(make-octet-vector 8) #xe849e8bb6ffe2567)
   (0 0 ,(make-octet-vector 1535) #xe74d1c0ab64b2afa)))

(deftest sip-hash ()
  (loop for (k0 k1 message expected-result) in +golden+ do
      (is (= (hash-2-4 message k0 k1) expected-result))))
