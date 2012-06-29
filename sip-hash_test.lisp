;;;; SipHash unit tests.

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

(defconst +expected-results+
  '(#x726fdb47dd0e0e31 #x74f839c593dc67fd #x0d6c8009d9a94f5a #x85676696d7fb7e2d #xcf2794e0277187b7
    #x18765564cd99a68d #xcbc9466e58fee3ce #xab0200f58b01d137 #x93f5f5799a932462 #x9e0082df0ba9e4b0
    #x7a5dbbc594ddb9f3 #xf4b32f46226bada7 #x751e8fbc860ee5fb #x14ea5627c0843d90 #xf723ca908e7af2ee
    #xa129ca6149be45e5 #x3f2acc7f57c29bdb #x699ae9f52cbe4794 #x4bc1b3f0968dd39c #xbb6dc91da77961bd
    #xbed65cf21aa2ee98 #xd0f2cbb02e3b67c7 #x93536795e3a33e88 #xa80c038ccd5ccec8 #xb8ad50c6f649af94
    #xbce192de8a85b8ea #x17d835b85bbb15f3 #x2f2e6163076bcfad #xde4daaaca71dc9a5 #xa6a2506687956571
    #xad87a3535c49ef28 #x32d892fad841c342 #x7127512f72f27cce #xa7f32346f95978e3 #x12e0b01abb051238
    #x15e034d40fa197ae #x314dffbe0815a3b4 #x027990f029623981 #xcadcd4e59ef40c4d #x9abfd8766a33735c
    #x0e3ea96b5304a7d0 #xad0c42d6fc585992 #x187306c89bc215a9 #xd4a60abcf3792b95 #xf935451de4f21df2
    #xa9538f0419755787 #xdb9acddff56ca510 #xd06c98cd5c0975eb #xe612a3cb9ecba951 #xc766e62cfcadaf96
    #xee64435a9752fe72 #xa192d576b245165a #x0a8787bf8ecb74b2 #x81b3e73d20b49b6f #x7fa8220ba3b2ecea
    #x245731c13ca42499 #xb78dbfaf3a8d83bd #xea1ad565322a1a0b #x60e61c23a3795013 #x6606d7e446282b93
    #x6ca4ecb15c5f91e1 #x9f626da15c9625f3 #xe51b38608ef25f57 #x958a324ceb064572)
    "Expected hash values extracted the C reference implementation:
http://131002.net/siphash/siphash24.c")

(deftest test-sip-hash-2-4 ()
  (let ((input (make-octet-vector 64)))
    (dotimes (i (length input))
      (setf (aref input i) i))
    (loop for expected-result in +expected-results+
          for end from 0
          do (let ((result (hash-2-4 input #x0706050403020100 #x0f0e0d0c0b0a0908 :end end)))
               (is (= result expected-result))))))
