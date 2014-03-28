(define (square x)
  (* x x))
(define (make-odd x)
  (- (* 2 x) 1))

(define (cont-frac n d k)
  (define (frac i)
    (if (= i k)
	(/ (n k) (d k))
	(/ (n i) (- (d i) (frac (+ i 1))))))
  (frac 1))

(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1) x (square x)))
	     (lambda (i) (make-odd i))
	     k))



