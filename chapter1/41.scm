; Exercise 1.41
(define (inc a) (+ a 1))

(define (double p)
  (lambda (x) (p (p x))))



(((double (double double)) inc) 5)

;(double inc)                            +2
;(double (double inc))                   +4
;(double (double (double inc)))          +8
;(double (double (double (double inc)))) +16

; 21 is returned