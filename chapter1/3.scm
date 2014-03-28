#lang planet neil/sicp
#|
Exercise 1.3
|#

(define (square a) (* a a))

(define (sum-of-squares a b)
  (+ (square a) (square b)))

(and 1 0)

(define (largest-sum a b c) 
  (cond ((and (> a b) (> b c)) (sum-of-squares a b))
        ((and (> a c) (> c b)) (sum-of-squares a c))
        (else (sum-of-squares b c))))
