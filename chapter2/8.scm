(define (lower-bound intrvl)
  (min (car intrvl) (cdr intrvl)))
  
(define (upper-bound intrvl)
  (max (car intrvl) (cdr intrvl)))

(define (make-interval a b)
  (cons a b))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))