
; This procedure seems to reduce each benchmark
; in half, since half of the numbers previously
; tested in 1-22 are no longer being tested here.
(define (next n)
  (cond ((= n 2) 3)
        (else
         (+ n 2))))
;----------------------------------------------------
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
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder a b) 0))

(define (square n)
  (* n n))
;--------------------------------------------------------
; 1-22 IMPLEMENTATION
;1000000009 *** 11000
;1000000021 *** 9000
;1000000033 *** 9000
;1000000087 *** 8000
;...

; 1-23 IMPLEMENTATION
;1000000009 *** 6000
;1000000021 *** 5000
;1000000033 *** 6000
;1000000087 *** 5000
;...
