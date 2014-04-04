;;Exercise 1-44
(load "43.scm")


(define (smooth f)
  ;This procedure takes as input a procedure that computes f
  ;and returns a procedure that computes the smoothed f.
  (define dx 0.00001)
  (lambda (x)
    (/ (+ (f x) (f (- x dx)) (f (+ x dx))) 3)))

;;[...] generate the n-fold smoothed function of any given
;;function using smooth and repeat from exercise 1.43.
(define (n-smoothed f n)
  ((repeated smooth n) f))

;Example case
(define (square x) (* x x))
((n-smoothed square 4) 3)
