;; Exercise 2.2

(define (average a b)
  (/ (+ a b) 2))

;;; A line is represented by a pair of points
(define (make-segment point-a point-b)
  (cons point-a point-b))
(define (start-segment line)
  (car line))
(define (end-segment line)
  (cdr line))

;;; A point is represented by a pair of numbers
(define (make-point x y)
  (cons x y))
(define (x-point point)
  (car point))
(define (y-point point)
  (cdr point))


;;; Return the midpoint of a line
(define (midpoint-segment line)
  ;; Use the line and point selectors we've
  ;; defined above to make a midpoint.
  (make-point (average
	       (x-point (start-segment line))
	       (x-point (end-segment line)))
	      (average
	       (y-point (start-segment line))
	       (y-point (end-segment line)))))
  
;;; Simple display procedure
(define (print-point p)
  (newline)			     
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
  

;;; Let's test this out
(define a (make-point 4 5))
(define b (make-point 12 10))
(define line1 (make-segment a b))
(print-point (midpoint-segment line1))

