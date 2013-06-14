#lang planet neil/sicp
;Here we must show that sum and product are both special cases of a more
;general notion called accumulate that combines a collection of terms, using
;some general accumulation function.
;------------------------------------------------------------------------------
; a. recursive implementation
(define (accumulate combiner null-value term a next b)
  (if (> a b)
   null-value
   (combiner (term a)
             (accumulate combiner null-value term (next a) next b))))

;------------------------------------------------------------------------------
; b. iterative implementation
(define (it-accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))
;------------------------------------------------------------------------------
; example execution using the higher order procedures above
(define (fact n)
  (define (identity k) k)
  (it-accumulate * 1 identity 1 inc n))
;------------------------------------------------------------------------------
; necessary stuff for the example
(define (inc n) (+ n 1))