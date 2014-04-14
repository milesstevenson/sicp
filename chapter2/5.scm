(define (cons23 a b)
  (* (expt 2 a) (expt 3 b)))

(define (car23 n)
  (define (car23-helper m count)
    (if (even? m)
	(car23-helper (/ m 2) (+ 1 count))
	count))
  (car23-helper n 0))
  

(define (cdr23 n)
  (define (cdr23-helper m count)
    (if (= (modulo m 3) 0)
	(cdr23-helper (/ m 3) (+ 1 count))
	count))
  (cdr23-helper n 0))