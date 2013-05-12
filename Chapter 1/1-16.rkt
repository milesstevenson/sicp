#lang planet neil/sicp

;SICP exercise 1-16
; O(logn) iterative solution to b^n problem.
(define (expt b n)
  (fast-expt-itr 1 b n))
(define (fast-expt-itr a b count)
  (cond ((= count 0) a)
        ((not (even? count))
         (fast-expt-itr (* a b) b (- count 1)))
        (else
         (fast-expt-itr a (square b) (/ count 2)))))

(define (square n) (* n n))
(define (even? n)
  (= (remainder n 2) 0))