;; Exercise 1.37



(define (cont-frac n d k)
  (define (frac i)
    (if (= k i)
	(/ (n i) (d i))
	(/ (n i) (+ (d i) (frac (+ i 1))))))
  (frac 1))



(define (cont-frac-it n d k)
  (define (frac-itr sol i)
    (if (= i 0)
	sol
	(frac-itr (/(n i) (+ (d i) sol)) (- i 1))))
  (frac-itr 0 k))
