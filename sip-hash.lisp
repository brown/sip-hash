;;;; SipHash, a cryptographically strong family of hash functions designed by
;;;; Jean-Philippe Aumasson and Daniel J. Bernstein.  (http://131002.net/siphash/)

;;;; Author: Robert Brown (robert.brown@gmail.com)

(in-package #:sip-hash)
(declaim #.*optimize-fast-unsafe*)

(defmacro mod-2^64 (x) `(logand ,x #.(ldb (byte 64 0) -1)))
(defmacro +-u64 (x y) `(mod-2^64 (+ ,x ,y)))
(defmacro ash-u64 (x y) `(mod-2^64 (ash ,x ,y)))

(define-modify-macro incf-u64 (x) +-u64 "Increment, modulo 2^64, PLACE by X.")
(define-modify-macro logiorf (x) logior)
(define-modify-macro logxorf (x) logxor)
(define-modify-macro rotatef-u64 (x) rotate-u64 "Rotate 64-bit PLACE left by X bit positions.")

(defmacro case-fall-through (key-form &body cases)
 "Equivalent to CASE, but control falls through from one case form to the next.
Use (RETURN) to exit the CASE-FALL-THROUGH form."
 (let* ((end (gensym "END"))
        (go-cases ())
        (body ()))
   (loop for (key . forms) in cases do
     (let ((label (gensym "L")))
       (push `(,key (go ,label)) go-cases)
       (push label body)
       (dolist (form forms) (push form body))))
   `(block nil
      (tagbody
         (case ,key-form ,@(reverse go-cases))
         (return)
         ,@(reverse body)
         ,end))))

(declaim (inline rotate-u64))

(defun rotate-u64 (x count)
  "Rotates the unsigned 64-bit integer X left by COUNT bit positions."
  (declare (type uint64 x)
           (type (integer 0 63) count))
  #+sbcl
  (sb-rotate-byte:rotate-byte count (byte 64 0) x)
  #-sbcl
  (logior (ash x count) (mod-2^64 (ash x (- count 64)))))

(declaim (inline load-64))

(defun load-64 (octets position)
  "Returns the unsigned 64-bit integer stored as 8 octets in little-endian form
starting at POSITION in OCTETS."
  (declare (type octet-vector octets)
           (type vector-index position))
  (logior (aref octets position)
          (ash (aref octets (+ position 1)) 8)
          (ash (aref octets (+ position 2)) 16)
          (ash (aref octets (+ position 3)) 24)
          (ash (aref octets (+ position 4)) 32)
          (ash (aref octets (+ position 5)) 40)
          (ash (aref octets (+ position 6)) 48)
          (ash (aref octets (+ position 7)) 56)))

(defmacro sip-round (v0 v1 v2 v3)
  `(progn (incf-u64 ,v0 ,v1)        (incf-u64 ,v2 ,v3)
          (rotatef-u64 ,v1 13)      (rotatef-u64 ,v3 16)
          (logxorf ,v1 ,v0)         (logxorf ,v3 ,v2)
          (rotatef-u64 ,v0 32)
          (incf-u64 ,v2 ,v1)        (incf-u64 ,v0 ,v3)
          (rotatef-u64 ,v1 17)      (rotatef-u64 ,v3 21)
          (logxorf ,v1 ,v2)         (logxorf ,v3 ,v0)
          (rotatef-u64 ,v2 32)))

(defmacro define-sip-hash (function-name compress-rounds finalization-rounds documentation)
  `(defun ,function-name (octets k0 k1 &key (start 0) end)
     ,documentation
     (declare (type octet-vector octets)
              (type uint64 k0 k1)
              (type vector-index start)
              (type (or null vector-index) end))
     (let ((v0 (logxor k0 #x736f6d6570736575))
           (v1 (logxor k1 #x646f72616e646f6d))
           (v2 (logxor k0 #x6c7967656e657261))
           (v3 (logxor k1 #x7465646279746573))
           (index start))
       (unless end (setf end (length octets)))
       ;; Compress each 64-bit message block.
       (loop while (<= index (- end 8)) do
         (let ((m (load-64 octets index)))
           (logxorf v3 m)
           ,@(make-list compress-rounds :initial-element '(sip-round v0 v1 v2 v3))
           (logxorf v0 m)
           (incf index 8)))
       ;; Compress the last message block.
       (let ((last-m (ash-u64 (- end start) 56)))
         (case-fall-through (- end index)
           (7 (logiorf last-m (ash (aref octets (+ index 6)) 48)))
           (6 (logiorf last-m (ash (aref octets (+ index 5)) 40)))
           (5 (logiorf last-m (ash (aref octets (+ index 4)) 32)))
           (4 (logiorf last-m (ash (aref octets (+ index 3)) 24)))
           (3 (logiorf last-m (ash (aref octets (+ index 2)) 16)))
           (2 (logiorf last-m (ash (aref octets (+ index 1)) 8)))
           (1 (logiorf last-m (aref octets index))))
         (logxorf v3 last-m)
         ,@(make-list compress-rounds :initial-element '(sip-round v0 v1 v2 v3))
         (logxorf v0 last-m))
       ;; Finalization.
       (logxorf v2 #xff)
       ,@(make-list finalization-rounds :initial-element '(sip-round v0 v1 v2 v3))
       (logxor v0 v1 v2 v3))))

(define-sip-hash hash-2-4 2 4
  "Returns SipHasp-2-4 hash code for positions START through END of OCTETS,
using the initial state stored in K0 and K1.")
(define-sip-hash hash-4-8 4 8
  "Returns SipHasp-4-8 hash code for positions START through END of OCTETS,
using the initial state stored in K0 and K1.")
