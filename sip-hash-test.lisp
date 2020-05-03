;;;; SipHash unit tests.

;;;; Author: Robert Brown <robert.brown@gmail.com>

(in-package #:common-lisp)

(defpackage #:sip-hash-test
 (:documentation "Test code in the SIP-HASH package.")
 (:use #:common-lisp
       #:com.google.base
       #:hu.dwim.stefil
       #:sip-hash)
 (:export #:test-sip-hash))

(in-package #:sip-hash-test)

(defsuite (test-sip-hash :in root-suite) ()
 (run-child-tests))

(in-suite test-sip-hash)

(defconst +expected-hash-values-64+
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
    "Expected 64-bit hash values extracted from the C reference implementation
available here: https://github.com/veorq/SipHash")

(defconst +expected-hash-values-128+
  '((#xe6a825ba047f81a3 #x930255c71472f66d) (#x44af996bd8c187da #x45fc229b11597634)
    (#xc75da4a48d227781 #xe4ff0af6de8ba3fc) (#x4ea967520cb6709c #x51ed8529b0b6335f)
    (#xaf8f9c2dc16481f8 #x7955cd7b7c6e0f7d) (#x886f778059876813 #x27960e69077a5254)
    (#x1386208b33caee14 #x5ea1d78f30a05e48) (#x53c1dbd8beebf1a1 #x3982f01fa64ab8c0)
    (#x61f55862baa9623b #xb49714f364e2830f) (#xabbad90a06994426 #xed716dbb028b7fc4)
    (#x56691478c30d1100 #xbafbd0f3d34754c9) (#x77666b3868c55101 #x18dce5816fdcb4a2)
    (#x58f35e9066b226d6 #x25c13285f64d6382) (#x108bc0e947e26998 #xf752b9c44f9329d0)
    (#x9cded766aceffc31 #x024949e45f48c77e) (#x11a8b03399e99354 #xd9c3cf970fec087e)
    (#xbb54b067caa4e26e #x77052385bf1533fd) (#x98b88d73e8063d47 #x4077e47ac466c054)
    (#x8548bf23e4e526a4 #x23f7aefe81a44d29) (#xb0fa65cf31770178 #xb12e51528920d574)
    (#x7390223f83fc259e #xeb3938e8a544933e) (#x215a52be5a498e56 #x121d073ecd14228a)
    (#x9a6bd15245b5294a #xae0aff8e52109c46) (#xe0f5a9d5dd84d1c9 #x1c69bf9a9ae28ccf)
    (#xd850bd78ae79b42d #xad32618a178a2a88) (#x7b445e2d045fce8e #x6f8f8dcbeab95150)
    (#xe807c3b3b4530b9c #x661f147886e0ae7e) (#xe4eaa669af48f2ab #x94eb9e122febd3bf)
    (#x884b576816da6406 #xf4ae587302f335b9) (#xe97d33bfc49d4baa #xb76a7c463cfdd40c)
    (#xde6baf1f477f5cea #x87226d68d4d71a2b) (#xfcfa233218b03929 #x353dc4524fde2317)
    (#x3efcea5eca56397c #x68eb4665559d3e36) (#x321cf0467107c677 #xcfffa94e5f9db6b6)
    (#xdf7e84b86c98a637 #xde549b30f1f02509) (#xf9a8a99de6f005a7 #xc88c3c922e1a2407)
    (#x4648c4291f7dc43d #x11674f90ed769e1e) (#x1a0efce601bf620d #x2b69d3c551473c0d)
    (#x9e667cca8b46038c #xb5e7be4b085efde4) (#x9c2caf3bb95b8a52 #xd92bd2d0e5cc7344)
    (#xad5dc9951e306adf #xd83b91c6c80cae97) (#x397f852c90891180 #xdbb6705e289135e7)
    (#xbb31c2c96a3417e6 #x5b0ccacc34ae5036) (#xaa21b7ef3734d927 #x89df5aecdc211840)
    (#x785e9ced9d7d2389 #x4273cc66b1c9b1d8) (#x657d5ebf91806d4a #x4cb150a294fa8911)
    (#x89aee75560f9330e #x022949cf3d0efc3f) (#xd1190b722b431ce6 #x1b1563dc4bd8c88e)
    (#xcf82f749f5aee5f7 #x169b2608a6559037) (#x4fa5b7d00f038d43 #x03641a20adf237a8)
    (#xe304bf4feed390a5 #x3f4286f2270d7e24) (#xc493fe72a1c1e25f #x38f5f9ae7cd35cb1)
    (#x6eb306bd5c32972c #x7c013a8bd03d13b2) (#x94ca6b7a2214c892 #x9ed32a009f65f09f)
    (#x8c32d80b1150e8dc #x871d91d64108d5fb) (#x1279dac78449f167 #xda832592b52be348)
    (#xe94ed572cff23819 #x362a1da96f16947e) (#xfe49ed46961e4874 #x8e6904163024620f)
    (#xd8d6a998dea5fc57 #x1d8a3d58d0386400) (#xbe1cdcef1cdeec9f #x595357d9743676d4)
    (#x53f128eb000c04e3 #x40e772d8cb73ca66) (#xfe1d836a9a009776 #x7a0f6793591ca9cc)
    (#xa067f52123545358 #xbd5947f0a447d505) (#x4a83502f77d15051 #x7cbd3f979a063e50))
    "Expected 128-bit hash values extracted from the C reference implementation
available here: https://github.com/veorq/SipHash")

(deftest expected-hash-values-64 ()
  (let ((input (make-octet-vector 64 :initial-contents (loop for i below 64 collect i))))
    (loop for expected in +expected-hash-values-64+
          for end from 0
          do (let ((result (hash-64-2-4 input #x0706050403020100 #x0f0e0d0c0b0a0908 :end end)))
               (is (= result expected))))))

(deftest expected-hash-values-128 ()
  (let ((input (make-octet-vector 64 :initial-contents (loop for i below 64 collect i))))
    (loop for (e0 e1) in +expected-hash-values-128+
          for end from 0
          do (multiple-value-bind (h0 h1)
                 (hash-128-2-4 input #x0706050403020100 #x0f0e0d0c0b0a0908 :end end)
               (is (= h0 e0))
               (is (= h1 e1))))))

(deftest argument-processing-64 ()
  (let ((octets (string-to-utf8-octets "hello world")))
    (is (= (hash-64-2-4 octets 0 0 :end 5) (hash-64-2-4 (subseq octets 0 5) 0 0)))
    (is (= (hash-64-2-4 octets 0 0 :start 6) (hash-64-2-4 (subseq octets 6) 0 0)))
    (is (= (hash-64-4-8 octets 0 0 :end 5) (hash-64-4-8 (subseq octets 0 5) 0 0)))
    (is (= (hash-64-4-8 octets 0 0 :start 6) (hash-64-4-8 (subseq octets 6) 0 0)))))

(deftest argument-processing-128 ()
  (let ((octets (string-to-utf8-octets "hello world")))
    (is (equal (multiple-value-list (hash-128-2-4 octets 0 0 :end 5))
               (multiple-value-list (hash-128-2-4 (subseq octets 0 5) 0 0))))
    (is (equal (multiple-value-list (hash-128-2-4 octets 0 0 :start 6))
               (multiple-value-list (hash-128-2-4 (subseq octets 6) 0 0))))
    (is (equal (multiple-value-list (hash-128-4-8 octets 0 0 :end 5))
               (multiple-value-list (hash-128-4-8 (subseq octets 0 5) 0 0))))
    (is (equal (multiple-value-list (hash-128-4-8 octets 0 0 :start 6))
               (multiple-value-list (hash-128-4-8 (subseq octets 6) 0 0))))))
