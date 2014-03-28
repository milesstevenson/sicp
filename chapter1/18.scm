
;SICP exercise 1-18
; Re-implement O(logn) iterative solution to a*b problem
; using double and half as primitives.
(define (* a b)
  (*-iter a b a 0))
(define (*-iter a b prod c)
  (cond ((= b 1)
         (+ prod c))
        ((not (even? b))
         (*-iter a (- b 1) prod (+ c a)))
        (else
         (*-iter a (half b) (double prod) c))))


(define (double n)
  (+ n n))

(define (half n)
  (/ n 2))

(define (even? n)
  (= (remainder n 2) 0))

        
