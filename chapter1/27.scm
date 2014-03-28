;> (carmic-test 561)
;#t
;> (carmic-test 1105)
;#t
;> (carmic-test 1729)
;#t
;> (carmic-test 2465)
;#t
;> (carmic-test 2821)
;#t
;> (carmic-test 6601)
;#t

; This exercise shows that the Fermat test isn't sufficient for all
; prime numbers through passing example Carmichael numbers as prime.

(define (carmic-test n)
  (test-it n (- n 1)))

(define (test-it n a)
  (cond ((= a 0) true)
        ((fermat-test n a) (test-it n (- a 1)))
        (else false)))

(define (fermat-test n times)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it times))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (even? n)
  (= (remainder n 2) 0))

(define (square m)
  (* m m))
