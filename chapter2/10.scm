(define (make-interval a b) (cons a b))

(define (lower-bound intrvl)
  (min (car intrvl) (cdr intrvl)))
  
(define (upper-bound intrvl)
  (max (car intrvl) (cdr intrvl)))

(define (div-interval x y)
  (if (or (= (upper-bound y) 0) (= (lower-bound y) 0))
      (error "You can't divide by zero." y)
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))
