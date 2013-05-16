#lang planet neil/sicp

;1000000009 *** 11000
;1000000021 *** 9000
;1000000033 *** 9000
;1000000087 *** 8000
;Prime tests finished.
;
;10000000033 *** 25000
;10000000061 *** 25000
;10000000069 *** 25000
;Prime tests finished.
;
;100000000019 *** 86000
;100000000057 *** 80000
;100000000063 *** 80000
;Prime tests finished.

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time) n)))

(define (report-prime elapsed-time n)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time))
;--------------------------------------------------------------
(define (prime? n)
  (= n (smallest-divisor n)))

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
;; It's difficult to determine how well 100,000 and 1,000,000
;; support O(sqrt(n)) due to how quickly our modern computers
;; execute these procedures.
;;
;; It does become obvious, as you use sufficiently large numbers,
;; that the procedures run in time proportional to the number of
;; steps required by the computer (how big n is).

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
        