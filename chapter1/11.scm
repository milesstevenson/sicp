;; Tree Recursion 
(define (f_1 n)
  (if (< n 3)
      n
      (+ (f_1 (- n 1)) 
         (* 2 (f_1 (- n 2)))
         (* 3 (f_1 (- n 3))))))
         
;; Iteration
(define (f_2 n)
  (if (< n 3)
      n
      (f-iter 2 1 0 n)))
  
(define (f-iter a b c count)
  (if (= count 2)
      a
      (f-iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))
