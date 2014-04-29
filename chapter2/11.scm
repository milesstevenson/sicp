(define (make-interval a b) (cons a b))

(define (lower-bound intrvl)
  (min (car intrvl) (cdr intrvl)))
  
(define (upper-bound intrvl)
  (max (car intrvl) (cdr intrvl)))

;;;Original mul-interval
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

;;;By testing the signs of the endpoints of the intervals, it is
;;;possible to break mul-interval into nine cases, only one of which
;;;requires more than two multiplications.
(define (mul-interval-rewrite x y)
  (let ((x-hi (upper-bound x))
        (x-lo (lower-bound x))
        (y-hi (upper-bound y))
        (y-lo (lower-bound y)))

    (cond ((and (>= x-hi 0) (>= x-lo 0))
           ;;Cases handled:
           ;; +,+  +,+
           ;;      +,-
           ;;      -,-
           (if (and (>= y-hi 0) (>= y-lo 0))
               (make-interval (* x-hi y-hi) (* x-lo y-lo)))
           (if (and (>= y-hi 0) (<= y-lo 0))
               (make-interval (* x-hi y-hi) (* x-hi y-lo)))
           (if (and (<= y-hi 0) (<= y-lo 0))
               (make-interval (*x-hi y-hi) (*x-hi y-lo ))))
          
          ((and (>= x-hi 0) (<= x-lo 0))
           ;;Cases handled:
           ;; +,-  +,+
           ;;      +,-
           ;;      -,-
           (if (and (>= y-hi 0) (>= y-lo 0))
               (make-interval (* x-hi y-hi) (* x-lo y-hi)))
           (if (and (>= y-hi 0) (<= y-lo 0))
               (make-interval (max (* x-lo y-lo) (* x-hi y-hi))
                              (min (* x-lo y-hi) (* x-hi y-lo))))
           (if (and (<= y-hi 0) (<= y-lo 0))
               (make-interval (* x-lo y-lo) (* x-hi y-lo))))

          ((and (<= x-hi 0) (<= x-lo 0))
           ;;Cases handled:
           ;; -,-  +,+
           ;;      +,-
           ;;      -,-
           (if (and (>= y-hi 0) (>= y-lo 0))
               (make-interval (* x-lo y-lo) (* x-hi y-hi)))
           (if (and (>= y-hi 0) (<= y-lo 0))
               (make-interval (* x-lo y-lo) (* y-hi x-lo)))
           (if (and (<= y-hi 0) (<= y-lo 0))
               (make-interval (* x-lo y-lo) (* x-hi y-hi)))))))

