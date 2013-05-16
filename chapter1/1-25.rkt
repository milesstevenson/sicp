#lang planet neil/sicp
; Initially I thought Alyssa's idea and implementation was ideal. The fast-exp
; algorithm runs in O(log n) time, afterall, with minimal deffered operations.

; The answer is that Alyssa P. Hacker's assumptions are wrong. The procedure
; she has given would not serve well as a fast prime tester. The implementation
; given by the authors is able to handle much larger numbers because of the many
; deffered operations that hang around until the base case is reached. 

; Also we don't have to return (a^n) in the book's original implementation. The
; value returned will probably usually be in the vicinity of n alone -- this is
; valuable for exceptionally large numbers.

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
      (report-prime (- (runtime) start-time) n)))

(define (report-prime elapsed-time n)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time))
;--------------------------------------------------------------
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (fermat-test n)
  (define (try-it a)
  (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (expmod base exp m)
  (remainder (fast-exp base exp) m))
  ;(cond ((= exp 0) 1)
  ;      ((even? exp)
 ;        (remainder (square (expmod base (/ exp 2) m))
  ;                  m))
  ;      (else
  ;       (remainder (* base (expmod base (- exp 1) m))
  ;                  m))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square n)
  (* n n))
;----------------------------------------------------------------
(define (search-for-primes start end)
  (if (even? start)
      (find-primes (inc start) end)
      (find-primes start end)))

(define (find-primes start end)
  (cond ((not (> start end)) 
              (cond ((even? start) 
                     (timed-prime-test (+ start 1)) 
                     (find-primes (+ start 1) end))
                    (else
                     (timed-prime-test (+ start 2)) 
                     (find-primes (+ start 2) end))))
        (else
         (newline)
         (display "Prime tests finished."))))
        
;-------------------------------------------------------------------
(define (fast-exp b n)
  (cond ((= n 0) 1)
        ((even? n)
         (square (fast-exp b (/ n 2))))
        (else
         (* b (fast-exp b (- n 1))))))