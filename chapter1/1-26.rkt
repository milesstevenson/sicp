#lang planet neil/sicp
;1-26  

; I think it's similar to the example of the tree structure of fibonacci in the 
; second lecture where the same value is computed an unecessary amount of times,
; when it could just be computed once.

;The implementation by Louis computes 
; (+ (/ n 2) (/ n 2)) = (+ (+ (/ n 4) (/ n 4))  (+ (/ n 4) (/ n 4))) = ... = (* 2 (/ n 2)) = O(n)

;The implementation by the book
; Instead of dividing the problem into two subproblems on each call, the problem is divided
; into ONE subproblem of half its size.

;Louis Reasoner's implementation
;(expmod 2 3 3)
;(remainder (* 2 (expmod 2 2 3)) 3)
;(remainder (* 2 (remainder (* (expmod 2 1 3) (expmod 2 1 3)) 3)) 3)
;(remainder (* 2 (remainder (* (remainder (* 2 (expmod 2 0 3)) 3) (remainder (* 2 (expmod 2 0 3)) 3)) 3)) 3)
;(remainder (* 2 (remainder (* (remainder (* 2 1) 3) (remainder (* 2 1) 3)) 3)) 3)
;(remainder (* 2 (remainder (* (remainder 2 3) (remainder 2 3)) 3)) 3)
;(remainder (* 2 (remainder (* 2 2) 3)) 3)
;(remainder (* 2 (remainder 4 3)) 3)
;(remainder (* 2 1) 3)
;(remainder 2 3)
;2

;The book's implementation
;(expmod 2 3 3)
;(remainder (* 2 (expmod 2 2 3)) 3)
;(remainder (* 2 (remainder (square (expmod 2 1 3) 3)))) 3)
;(remainder (* 2 (remainder (square (remainder (* 2 (expmod 2 0 3)) 3)) 3)) 3)
;(remainder (* 2 (remainder (square (remainder (* 2 1) 3)) 3)) 3)
;(remainder (* 2 (remainder (square (remainder 2 3)) 3)) 3)
;(remainder (* 2 (remainder (square 2) 3)) 3)
;(remainder (* 2 (remainder 4 3)) 3)
;(remainder (* 2 1) 3)
;(remainder 2 3)
;2