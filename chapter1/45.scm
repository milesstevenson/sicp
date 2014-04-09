;; 1.45
;
; average damps| 1|1|2|2|2|2|3|3|3|......
;-------------------------------------------------------------------- 
; nth root     | 2|3|4|5|6|7|8|9|10|....
;;;;;;;;;;;;;;;;;;


(define (average-damp f)
  (lambda (x) (average x (f x))))
(define (average a b)
  (/ (+ a b) 2))

(define tolerance 0.000001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

(define (compose f g)
  (lambda (x)
    (f (g x))))
(define (repeated f n)
  (if (= n 1) f
      (compose f (repeated f (- n 1)))))


(define (nrt x n)
  ; From testing the output of when average-damp fails,
  ; the conclusion is that fixed-point must use average-damp
  ; k times for floor(2^k=n), or floor(log(n)/log(2)) times.
  (fixed-point 
   ((repeated average-damp (floor (/ (log n) (log 2))))
    (lambda (y) (/ x (expt y (- n 1)))))
   1.0))