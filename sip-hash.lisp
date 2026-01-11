;;;; SipHash, a cryptographically strong family of hash functions designed by
;;;; Jean-Philippe Aumasson and Daniel J. Bernstein.  (https://en.wikipedia.org/wiki/SipHash)

;;;; Author: Robert Brown <robert.brown@gmail.com>

(in-package #:sip-hash)

(defmacro mod-2^64 (x) `(logand ,x #.(ldb (byte 64 0) -1)))
(defmacro u64+ (x y) `(mod-2^64 (+ ,x ,y)))
(defmacro ash-u64 (x y) `(mod-2^64 (ash ,x ,y)))

(define-modify-macro incf64 (x) u64+ "Increment, modulo 2^64, PLACE by X.")
(define-modify-macro logiorf (x) logior "Logically inclusive or PLACE by X.")
(define-modify-macro logxorf (x) logxor "Logically exclusive or PLACE by X.")
(define-modify-macro rotatef-left-64 (x) rotate-left-64
  "Rotate 64-bit PLACE left by X bit positions.")

(declaim (inline rotate-left-64))

(defun rotate-left-64 (x count)
  "Rotates the unsigned 64-bit integer X left by COUNT bit positions."
  (declare (type uint64 x)
           (type (integer 0 63) count))
  #+sbcl
  (sb-rotate-byte:rotate-byte count (byte 64 0) x)
  #-sbcl
  (logior (mod-2^64 (ash x count)) (ash x (- count 64))))

(defmacro sip-round (v0 v1 v2 v3)
  `(progn (incf64 ,v0 ,v1)              (incf64 ,v2 ,v3)
          (rotatef-left-64 ,v1 13)      (rotatef-left-64 ,v3 16)
          (logxorf ,v1 ,v0)             (logxorf ,v3 ,v2)
          (rotatef-left-64 ,v0 32)
          (incf64 ,v2 ,v1)              (incf64 ,v0 ,v3)
          (rotatef-left-64 ,v1 17)      (rotatef-left-64 ,v3 21)
          (logxorf ,v1 ,v2)             (logxorf ,v3 ,v0)
          (rotatef-left-64 ,v2 32)))

(defmacro define-sip-hash (function-name compress-rounds finalization-rounds double documentation)
  "Defines a function called FUNCTION-NAME that implements the SipHash hash
function that performs COMPRESS-ROUNDS compression rounds and
FINALIZATION-ROUNDS finalization rounds.  The defined SipHash function is
documented with a DOCUMENTATION string.  When DOUBLE is true, a 128-bit hash
result is produced."
  (let ((compress (make-list compress-rounds :initial-element '(sip-round v0 v1 v2 v3)))
        (finalize (make-list finalization-rounds :initial-element '(sip-round v0 v1 v2 v3))))
    `(defun ,function-name (octets k0 k1 &key (start 0) (end (length octets)))
       ,documentation
       (declare (type octet-vector octets)
                (type uint64 k0 k1)
                (type vector-index start end))
       (let ((v0 (logxor k0 #x736f6d6570736575))
             (v1 (logxor k1 #x646f72616e646f6d))
             (v2 (logxor k0 #x6c7967656e657261))
             (v3 (logxor k1 #x7465646279746573))
             (index start))
         ,(when double '(logxorf v1 #xee))
         ;; Compress each 64-bit message block.
         (loop while (<= index (- end 8)) do
           (let ((m (ub64ref/le octets index)))
             (logxorf v3 m)
             ,@compress
             (logxorf v0 m)
             (incf index 8)))
         ;; Compress the last message block.
         (let ((last-m (ash-u64 (- end start) 56)))
           (case (- end index)
             (7
              (logiorf last-m (ub32ref/le octets index))
              (logiorf last-m (ash (ub16ref/le octets (+ index 4)) 32))
              (logiorf last-m (ash (aref octets (+ index 6)) 48)))
             (6
              (logiorf last-m (ub32ref/le octets index))
              (logiorf last-m (ash (ub16ref/le octets (+ index 4)) 32)))
             (5
              (logiorf last-m (ub32ref/le octets index))
              (logiorf last-m (ash (aref octets (+ index 4)) 32)))
             (4 (logiorf last-m (ub32ref/le octets index)))
             (3
              (logiorf last-m (ub16ref/le octets index))
              (logiorf last-m (ash (aref octets (+ index 2)) 16)))
             (2 (logiorf last-m (ub16ref/le octets index)))
             (1 (logiorf last-m (aref octets index))))
           (logxorf v3 last-m)
           ,@compress
           (logxorf v0 last-m))
         ;; Finalization.
         (logxorf v2 ,(if (not double) #xff #xee))
         ,@finalize
         ,(if (not double)
              '(logxor v0 v1 v2 v3)
              `(let ((h0 (logxor v0 v1 v2 v3)))
                 (logxorf v1 #xdd)
                 ,@finalize
                 (let ((h1 (logxor v0 v1 v2 v3)))
                   (values h0 h1))))))))

(define-sip-hash hash-64-2-4 2 4 nil
  "Returns as an (unsigned-byte 64) the 64-bit SipHash-2-4 hash code for
positions START through END of OCTETS, using the initial state stored in K0 and
K1.")

(define-sip-hash hash-64-4-8 4 8 nil
  "Returns as an (unsigned-byte 64) the 64-bit SipHash-4-8 hash code for
positions START through END of OCTETS, using the initial state stored in K0 and
K1.")

(define-sip-hash hash-128-2-4 2 4 t
  "Returns as two (unsigned-byte 64) values the 128-bit SipHash-2-4 hash code
for positions START through END of OCTETS, using the initial state stored in K0
and K1.")

(define-sip-hash hash-128-4-8 4 8 t
  "Returns as two (unsigned-byte 64) values the 128-bit SipHash-4-8 hash code
for positions START through END of OCTETS, using the initial state stored in K0
and K1.")
