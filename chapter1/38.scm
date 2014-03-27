#| 
A = {1,2,1,1,4,1,1,6,1,1,8,1,1,10,....}

B = {2,5,8,11,14,17,20,...}

C = {2,4,6,8,10,12,...}

At every 3k-1 position, where k is positive,
there is an even number in set A.

How do we deal with this, though?

3(k+1)-1 = 3k + 2

Maybe we can use 3k+2 to help us.

|#

(define (cont-frac n d k)
  (define (frac i)
    (if (= k i)
	(/ (n i) (d i))
	(/ (n i) (+ (d i) (frac (+ i 1))))))
  (frac 1))

(define (divisible? a b)
  (= (modulo a b) 0))

;; If the remainder of(k+1)/3 = 0, then ((k+1)/3) * 2 will produce
;; the appropriate number for the kth position.

(define (approx-e k) 
  (if (divisible? (+ k 1) 3)
      (* (/ (+ k 1) 3) 2)
      1))

      

(+ 2 (cont-frac (lambda (i) 1.0) approx-e 20))

