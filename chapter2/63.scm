#lang racket
;;; A.
;;; These two procedures do produce the same result for every tree,
;;; and they produce the same results for all trees from figure 2.16

;;; B.
;;; tree->list-1 recurses down the left branch while tree->list2 recurses
;;; down the right. Append is an O(n) procedure and is used in tree->list-1
;;; while tree->list-2 only uses cons. So we can say with certainty that
;;; tree->list-2 grows more slowly in space and time.
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define A '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ())))) 
(define B '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
(define C '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))
