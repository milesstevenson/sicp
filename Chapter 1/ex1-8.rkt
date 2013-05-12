#lang planet neil/sicp 

(define (cubert x)
  (cubert-iter 1.0 x))

(define (cubert-iter guess x)
  (if (good-enough? guess x)
      guess
      (cubert-iter (improve guess x)
                 x)))
(define (good-enough? guess x)
  (< (abs (- guess (improve guess x))) 
     0.001))

(define (improve guess x)
       (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (square x) (* x x))
(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))
#|
This exercise was pretty similiar to the previous. The only difference is
that there's a new method of improving the guess.
|#