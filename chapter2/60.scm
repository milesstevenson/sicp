#lang racket
(provide element-of-set? adjoin-set union-set intersection-set)
;;; I cannot think of any situation where it would be ideal to allow for the
;;; representation of a set to have duplicate elements. Although adjoin-set
;;; and union-set have faster running times here they data which they are
;;; computing over can become excessively large from allowing duplicate elements.


;;; element-of-set? in the description of 2.59 and 2.60 are the same and have
;;; the same O(n) running time.
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((eq? x (car set)) true)
        (else
         (element-of-set? x (cdr set)))))

;;; adjoin-set in the description of 2.59 and 2.60 differ in that the former
;;; required O(n) time to see if x was already an element of the set. Because
;;; multiple elements of the same kind are allowed in a set, this procedure
;;; runs in O(1).
(define (adjoin-set x set)
  (cons x set))

;;; union-set in the description of 2.59 differs from 2.60 in that the former
;;; has a running time of O(n^2)because it checks set2 for duplicates of the
;;; prospective element from set1 to be adjoined with it.
;;;
;;; Here with the description of 2.60, duplicates in set2 do not need to be
;;; checked. The running time is O(n), since every element of set1 must be
;;; adjoined to set2.
(define (union-set set1 set2)
  (if (null? set1) 
      set2
      (union-set (cdr set1) (adjoin-set (car set1) set2))))

;;; intersection-set runs in O(n^2) time just as it does in 2.59
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) 
               (intersection-set (cdr set1) set2)))
        (else
         (intersection-set (cdr set1) set2))))