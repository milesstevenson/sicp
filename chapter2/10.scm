(define (make-interval a b) (cons a b))

(define (lower-bound intrvl)
  (min (car intrvl) (cdr intrvl)))
  
(define (upper-bound intrvl)
  (max (car intrvl) (cdr intrvl)))
(define (width intrvl)
  (/ (- (upper-bound intrvl) (lower-bound intrvl)) 2))

(define (spans-zero? intrvl)
  (= (width intrvl) 0))

(define (div-interval x y)
  (if (spans-zero? y)
      (error "You can't divide by zero." y)
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))
