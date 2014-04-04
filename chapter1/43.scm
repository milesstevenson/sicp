;;Exercise 1.43
(load "42.scm")

(define (repeated f n)
  ;This procedure takes as inputs a procedure that
  ;computes f and a positive integer n and returns
  ;the procedure that computes the nth repeated
  ;application of f.n
  (if (= n 1) f
      (compose f (repeated f (- n 1)))))
