(define (cons x y)
  (lambda (m) (m x y)))
(define (car z) (z (lambda (p q) p)))
(define (cdr z) (z (lambda (p q) q)))

#|
applicative-order evaluation

(cdr (cons 2 3))

(cdr (lambda (m) (m 2 3)))

((lambda (m) (m 2 3)) (lambda (p q) q))

((lambda ((lambda (p q) q))
   ((lambda (p q) q) 2 3))

(lambda (2 3) 3)

3
|#

#|
normal-order evaluation
(cdr (cons 2 3))

((cons (2 3)) (lambda (p q) q))

((lambda (m) (m 2 3)) (lambda (p q) q))

((lambda ((lambda (p q) q)) ((lambda (p q) q) 2 3)))

(lambda (2 3) 3)

3
|#
