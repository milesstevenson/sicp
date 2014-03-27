(define (square x)
  (* x x))
(define (make-odd x)
  (- (* 2 x) 1))

(define (tan-cf x k)
  (define (frac-i sol i)
    (if (= i 0)
	(/ sol x)
	(frac-i (/ (square x) (- (make-odd i) sol)) (- i 1))))
  (frac-i (square x) k))



