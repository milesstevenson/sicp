;; 1.46
; Having a bit of trouble reasoning about higher-order procedures
; Thankfully there are a lot of online resources.
;
; billthelizard provides a great explanation: 
;       http://www.billthelizard.com/2010/08/sicp-146-iterative-improvement.html

(define (iterative-improve adequate? update)
  (define (iter-imp guess)
    (if (adequate? guess)
	guess
	(iter-imp (update guess))))
  iter-imp)

(define (sqrt x)
  ((iterative-improve 
    (lambda (guess) (< (abs (- (square guess) x))
			   0.00001))
    (lambda (guess) (average guess (/ x guess))))
   1.0))


(define (fixed-point f first-guess)
  ((iterative-improve (lambda (guess) 
			(< (abs (- (f guess) guess)) 0.00001))
		      f)
   first-guess))