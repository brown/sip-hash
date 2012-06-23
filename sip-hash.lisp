;;;; SipHash, a cryptographically strong family of hash functions designed by
;;;; Jean-Philippe Aumasson and Daniel J. Bernstein.  (http://131002.net/siphash/)

;;;; Author: Robert Brown (robert.brown@gmail.com)

(in-package #:sip-hash)
(declaim #.*optimize-default*)

(defmacro mod-2^64 (x) `(logand ,x #xffffffffffffffff))

(defmacro u64+ (x y) `(mod-2^64 (+ ,x ,y)))
(defmacro ash64 (x y) `(mod-2^64 (ash ,x ,y)))

(defmacro incf64 (place x) `(setf ,place (u64+ ,place ,x)))
(defmacro logiorf (place x) `(setf ,place (logior ,place ,x)))

(defmacro case-fall-through (key-form &body cases)
 "Equivalent to CASE, but control \"falls-through\" from one case form to the
next. Use (RETURN) to exit the CASE-FALL-THROUGH."
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

(declaim (inline rotate-right-64))

(defun rotate-left-64 (x shift)
 (declare (type uint64 x)
          (type (integer 7 53) shift))
 #+sbcl
 (sb-rotate-byte:rotate-byte shift (byte 64 0) x)
 #-sbcl
 (logior (ash x shift) (mod-2^64 (ash x (- shift 64)))))

(declaim (inline load-64))

(defun load-64 (octets index)
 (declare (type octet-vector octets)
          (type vector-index index))
 (logior (aref octets index)
         (ash (aref octets (+ index 1)) 8)
         (ash (aref octets (+ index 2)) 16)
         (ash (aref octets (+ index 3)) 24)
         (ash (aref octets (+ index 4)) 32)
         (ash (aref octets (+ index 5)) 40)
         (ash (aref octets (+ index 6)) 48)
         (ash (aref octets (+ index 7)) 56)))

;; (defmacro sip-round (v0 v1 v2 v3)
;;   (macrolet ((left (x y rotation)
;;                `(progn (incf64 ,x ,y)
;;                        (setf ,y (logxor (rotate-left-64 ,y ,rotation) ,x))
;;                        (setf ,x (rotate-left-64 ,x 32))))
;;              (right (x y rotation)
;;                `(progn (incf64 ,x ,y)
;;                        (setf ,y (logxor (rotate-left-64 ,y ,rotation) ,x)))))
;;     `(progn (left ,v0 ,v1 13)
;;             (right ,v2 ,v3 16)
;;             (left ,v2 ,v1 17)
;;             (right ,v0 ,v3 21))))

(defmacro rotatef64 (place x) `(setf ,place (rotate-left-64 ,place ,x)))
(defmacro logxorf (place x) `(setf ,place (logxor ,place ,x)))

(defmacro sip-round (v0 v1 v2 v3)
  `(progn (incf64 ,v0 ,v1)      (incf64 ,v2 ,v3)
          (rotatef64 ,v1 13)    (rotatef64 ,v3 16)
          (logxorf ,v1 ,v0)     (logxorf ,v3 ,v2)
          (rotatef64 ,v0 32)
          (incf64 ,v2 ,v1)      (incf64 ,v0 ,v3)
          (rotatef64 ,v1 17)    (rotatef64 ,v3 21)
          (logxorf ,v1 ,v2)     (logxorf ,v3 ,v0)
          (rotatef64 ,v2 32)))

(defmacro define-sip-hash (function-name compress-rounds finalization-rounds)
  `(defun ,function-name (octets k0 k1 &key (start 0) (end (length octets)))
     (declare (type octet-vector octets)
              (type uint64 k0 k1)
              (type vector-index start end))
     (let ((v0 (logxor k0 #x736f6d6570736575))
           (v1 (logxor k1 #x646f72616e646f6d))
           (v2 (logxor k0 #x6c7967656e657261))
           (v3 (logxor k1 #x7465646279746573))
           (index start))
       ;; Compress each 64-bit message block.
       (loop while (<= index (- end 8)) do
         (let ((m (load-64 octets index)))
           (logxorf v3 m)
           ,@(make-list compress-rounds :initial-element '(sip-round v0 v1 v2 v3))
           (logxorf v0 m)
           (incf index 8)))
       ;; Compress the last message block.
       (let ((last-m (ash64 (- end start) 56)))
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

(define-sip-hash hash-2-4 2 4)
(define-sip-hash hash-4-8 4 8)
