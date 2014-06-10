#lang racket
(provide union-set)
;;; This solution runs in O(n) time. Big improvement just from using an ordered
;;; set data structure!
 (define (union-set set1 set2)
   (cond ((and (null? set1) (null? set2)) '())
         ((and (null? set1) (not (null? set2))) set2)
         ((and (null? set2) (not (null? set1))) set1)
         (else 
          (let ((x1 (car set1)) (x2 (car set2)))
            (cond ((> x1 x2) (cons x2 (union-set set1 (cdr set2))))
                  ((> x2 x1) (cons x1 (union-set (cdr set1) set2)))
                  (else
                   (= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2)))))))))
          