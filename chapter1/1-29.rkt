#lang planet neil/sicp
;We're implementing  Simpson's Rule for computing integrals here.
;(h/3) * [y_0 + 4y_1 + 2y_2 + ... + 4y_(n-1) + y_n]
;h = (b - a)/n
;y_k = a + k*h     IF k = 0, k = n
;    = 4*(a + k*h) IF k is odd
;    = 2*(a + k*h) IF k is even

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) 
           (sum term (next a) next b))))

(define (simpson-rule f a b n)
  (define h (/ (- b a) n))
  (define (y_ k)
    (f (+ a (* k h))))
  (define (term k)
    (cond ((or (= k 0) (= k n))
           (y_ k))
          ((even? k)
           (* 4 (y_ k)))
          (else
           (* 2 (y_ k)))))           
  (* (/ h 3)
     (sum term 0 inc n)))

;--------------------------------------------------
(define (inc n) (+ n 1))
(define (cube x)
  (* x x x))
(define (even? n)
  (= (remainder n 2) 0))
;--------------------------------------------------
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))