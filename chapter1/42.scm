; Exercise 1.42
(define (square x) (* x x))
(define (inc x) (+ x 1))
;((compose square inc) 6)
;Value: 49

(define (compose f g)
  (lambda (x)
    (f (g x))))