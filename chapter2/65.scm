#lang racket
;;; Both partial-tree and tree->list-2 run in O(n) time. Because of this, we can
;;; convert our sets to ordered lists, 2*O(n) time, execute union-set or inters-
;;; ection-set on the ordered list, O(n) time, and finally convert our list back
;;; to a balanced binary tree. All of this falling under O(n) time.
(define (union-set set1 set2)
  (define (union-set-list set1 set2)
    (cond ((and (null? set1) (null? set2)) '())
          ((and (null? set1) (not (null? set2))) set2)
          ((and (null? set2) (not (null? set1))) set1)
          (else 
           (let ((x1 (car set1)) (x2 (car set2)))
             (cond ((> x1 x2) (cons x2 (union-set-list set1 (cdr set2))))
                   ((> x2 x1) (cons x1 (union-set-list (cdr set1) set2)))
                   (else
                    (= x1 x2) (cons x1 (union-set-list (cdr set1) (cdr set2)))))))))
  (list->tree (union-set-list (tree->list-2 set1) (tree->list-2 set2))))

(define (intersection-set set1 set2)
  (define (intersection-set-list set1 set2)
  (if (or (null? set1) (null? set2)) 
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set-list (cdr set1)
                                          (cdr set2))))
              ((> x1 x2)
               (intersection-set-list set1 (cdr set2)))
              ((< x1 x2)
               (intersection-set-list (cdr set1) set2))))))
  (list->tree (intersection-set-list (tree->list-2 set1) (tree->list-2 set2))))

;;;=============================================================================
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))  
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))