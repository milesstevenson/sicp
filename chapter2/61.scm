#lang racket
(provide adjoin-set)
;;; With the unordered set data representation, the procedure adjoin-set needed
;;; to check, in the worse case, every element of set to ensure that x was not a
;;; redundant new member of the set.
;;;
;;; In this implementation of adjoin-set, where an ordered set data representation
;;; is assumed, the worst case is also O(n). On the average, though, we should
;;; expect to have to examine about half of the set so the average number of steps
;;; required will be about n/2.

(define (adjoin-set x set)
  (cond ((null? set) (cons x '()))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else
         (cons (car set) (adjoin-set x (cdr set))))))


