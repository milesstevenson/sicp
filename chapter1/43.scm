;;Exercise 1.43
(load "42.scm")

(define (repeated f n)
  ;This procedure takes as inputs a procedure that
  ;computes f and a positive integer n and returns
  ;the procedure that computes the nth repeated
  ;application of f.
  (lambda (x)
    (cond ((< n 1) x)
	  ((even? n) ((repeated f (- n 2)) ((compose f f) x)))
	  (else ((repeated f (- n 1)) (f x))))))
