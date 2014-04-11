;;; Exercise 2.1

(define (make-rat n d)
  (let ((g (gcd n d)))  
    (cond ((or (and (positive? n) (positive? d))
	       (and (negative? n) (negative? d)))
	   ;; Are both numerator and denominator positive?
	   ;; Are they both negative?
	   ;; If so, the rational number is positive.
	   (cons (abs (/ n g)) (abs (/ d g))))  
	  ((negative? d)
	   ;; Is the denominator only negative?
	   ;; If so, make the numerator negative instead.
	   (cons (* (/ n g) -1) (abs (/ d g))))
	  (else
	   ;; If none of the above cases are met, the numerator
	   ;; is the only negative number. Proceed as usual.
	   (cons (/ n g) (/ d g))))))
	   
	