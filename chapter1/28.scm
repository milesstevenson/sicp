
; Provided code from SICP
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (even? n)
  (= (remainder n 2) 0))

(define (square m)
  (* m m))

; Original expmod
;(define (expmod base exp m)
;  (cond ((= exp 0) 1)
;        ((even? exp)
;         (remainder (square (expmod base (/ exp 2) m))
;                    m))
;        (else
;         (remainder (* base (expmod base (- exp 1) m))
;                    m))))

; Slightly altered expmod
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (mill-rab-test (expmod base (/ exp 2) m) m)
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

; Millner-Rabin test
(define (mill-rab-test r m)
  (cond ((and (not (or (= r 1) (= r (- m 1))))
              (= (remainder (square r) m) 1))
         0)
        (else
         (square r))))
 
; If n is prime, then a^(- n 1) is congruent to 1 mod n.

; To test the primality of a number n by the Millner-Rabin test, we pick a
; random number a < n and raise a to the (n-1)st power modulo n using the
; expmod procedure.
;
; However, whenever we perform the squaring step in expmod, we check to see
; if we have discovered a "nontrivial square root of 1 mod n," that is, a number
; not equal to 1 or n-1 whose square is equal to 1 modulo n.
