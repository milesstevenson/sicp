#lang racket
;;; Evaluation of the substitution rule is from left to right. There are much
;;; more eloquent solutions available, but this gets the job done. This should
;;; be evaluated once and only once. 

(define f
  (let ((state 0))
    (lambda (number)
      (set! state (+ state 1))
      (cond ((and (>= 1 state) (equal? number 0))
             -1)
            ((and (>= 1 state) (equal? number 1))
             1)
            ((and (= state 2) (equal? number 1))
             1)
            ((and (= state 2) (equal? number 0))
             0)
            (else (error " State to large a number. Please rerun the program."))))))
            
 