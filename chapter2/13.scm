(define (mul-interval-rewrite x y)
  (let ((x-hi (upper-bound x))
        (x-lo (lower-bound x))
        (y-hi (upper-bound y))
        (y-lo (lower-bound y)))
    (cond ((and (>= x-hi 0)
                (>= x-lo 0)
                (>= y-hi 0)
                (>= y-lo 0))
           (make-interval (* x-hi y-hi) (* x-lo y-lo)))
          ((and (>= x-hi 0)
                (>= x-lo 0)
                (>= y-hi 0)
                (<= y-lo 0))
           (make-interval (* x-hi y-hi) (* x-hi y-lo)))
          ((and (>= x-hi 0)
                (>= x-lo 0)
                (<= y-hi 0)
                (<= y-lo 0))
           (make-interval (*x-hi y-hi) (*x-hi y-lo )))
          ((and (>= x-hi 0)
                (<= x-lo 0)
                (>= y-hi 0)
                (>= y-lo 0))
           (make-interval (* x-hi y-hi) (* x-lo y-hi)))
          ((and (>= x-hi 0)
                (<= x-lo 0)
                (>= y-hi 0)
                (<= y-lo 0))
           (make-interval
            (max (* x-lo y-lo) (* x-hi y-hi))
            (min (* x-lo y-hi) (* x-hi y-lo))))
          ((and (>= x-hi 0)
                (<= x-lo 0)
                (<= y-hi 0)
                (<= y-lo 0))
           (make-interval (* x-lo y-lo) (* x-hi y-lo)))
          ((and (<= x-hi 0)
                (<= x-lo 0)
                (>= y-hi 0)
                (>= y-lo 0))
           (make-interval (* x-lo y-lo) (* x-hi y-hi)))
          ((and (<= x-hi 0)
                (<= x-lo 0)
                (>= y-hi 0)
                (<= y-lo 0))
           (make-interval (* x-lo y-lo) (* y-hi x-lo)))
          ((and (<= x-hi 0)
                (<= x-lo 0)
                (<= y-hi 0)
                (<= y-lo 0))
           (make-interval (* x-lo y-lo) (* x-hi y-hi))))))


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
#|
2 error> (percent a)

;Value: .04285714285714283

2 error> (percent b)

;Value: .06666666666666662

2 error> (percent (mul-interval-rewrite a b))

;Value: .10921177587844252
;--------------------------------------
2 error> (percent c)

;Value: .35000000000000003

2 error> (percent d)

;Value: .25

2 error> (percent (mul-interval-rewrite c d))

;Value: .5517241379310344

|#


;;; Tests above tell us that the APPROXIMATE percentage tolerance of the product
;;; two intervals is (+ (percent a) (percent b)), where a and b are intervals with
;;; a small percentage tolerance.
(define (approx-mul-% a b)
  (+ (percent a) (percent b)))