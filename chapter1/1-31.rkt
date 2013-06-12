#lang planet neil/sicp
; We use part of Euler's infinite product proof for this.
; http://en.wikipedia.org/wiki/Wallis_product
 (define (pi-approx n)
   (define (pi-term k)
     (/ (* 4 (square k)) 
        (- (* 4 (square k)) 1)))
   (* 2 (it-product pi-term 1 inc n)))
;---------------------------------------------------------
 (define (fact n)
  (define (identity k) k)
  (it-product identity 1 inc n))
;--------------------------------------------------------- 
(define (inc n) (+ n 1))
(define (square n) (* n n))
;---------------------------------------------------------
;Part a is recursive.
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))
;---------------------------------------------------------
;Part b is iterative.
(define (it-product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))
