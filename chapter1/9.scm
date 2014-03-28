

;; Procedure 1 
(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

;; Recursive
;;(+ 2 1)
;;(inc (+ 1 1))
;;(inc (inc (+ 0 1)))
;;(inc (inc 1))
;;(inc 2)
;; 3

;; Procedure 2 
(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))

;; Iterative
;;(+ 2 1)
;;(+ 1 2)
;;(+ 0 3)
;; 3
