#lang planet neil/sicp

;SICP exercise 1-17
; Re-implement O(logn) iterative solution to a*b problem
; using double and half as primitives.


;; BOOK implementation
(define (*-book a b)
  (cond ((= b 0)
         0)
        (else
         (+ a (*-book a (- b 1))))))

(define (double n)
  (+ n n))

(define (half n)
  (/ n 2))

;; MY implementation
(define (* a b )
  (*_ a b a))
(define (*_ a count b)
  (cond ((= count 1) a)
        ((not (even? count))
         (+ a (*_ a (+ count (- 1)) b)))
        (else
         (*_ (double a) (half count) b))))
(define (even? n)
  (= (remainder n 2) 0))

        