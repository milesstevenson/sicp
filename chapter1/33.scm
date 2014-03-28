;Supposedly we can generalize even more than accumulate through introducing
;the notion of a filter on the terms to be combined.
;
;From my understanding, we are basically filtering the initial range of [a,b]
;and using it in our original accumulate procedure.

;PART A
(define (filtered-accumulate filter combiner null-value term a next b)       
  (cond ((> a b) null-value)
        ((filter a)
         (combiner (term a)
                   (filtered-accumulate filter combiner null-value term (next a) next b)))
        (else
         (filtered-accumulate filter combiner null-value term (next a) next b))))

;Code from exercise 1-21 for prime testing.
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (square n)
  (* n n))

; necessary stuff
(define (inc n) (+ n 1))

;Sum of prime squares (part a)
(define (sum-of-prime-squares a b)
  (cond ((= a 1)
         (filtered-accumulate prime? + 0 square (inc a) inc b))
        (else
         (filtered-accumulate prime? + 0 square a inc b))))
;------------------------------------------------------------------------------
;Code for the product of the range [a,b] where a_i is relatively prime to b.

;PART B
(define (product-relative-prime n)
  (define (relatively-prime? m)
    (= 1 (gcd n m)))
  (filtered-accumulate relatively-prime? * 1 identity 1 inc n))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (identity k) k)
