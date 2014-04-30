(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (lower-bound intrvl)
  (min (car intrvl) (cdr intrvl)))
  
(define (upper-bound intrvl)
  (max (car intrvl) (cdr intrvl)))

         
(define (make-interval a b)
  (cons a b))

(define (make-center-percent c p)
  (make-interval (+ (* c p) c) (- c (* c p))))

(define (percent inv)
  (/ (width inv) (center inv)))