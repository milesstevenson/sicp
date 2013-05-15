#lang planet neil/sicp

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time) 
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

(define (search-for-primes range)
  (find-primes range 0))

(define (find-primes range count)
  (cond ((= count 3) 
         (newline)
         (display "Finished!"))
    ((prime? range) ;; yes, not the most efficient.
     (timed-prime-test range)
     (find-primes (inc range) (inc count)))
  (else
   (find-primes (inc range) count))))
        