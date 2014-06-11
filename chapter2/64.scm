#lang racket
;;; a.
;;; Partial-tree keeps track of three main variables during the time that it's
;;; running. Those are left-tree, this-entry, and right-tree. All other variables
;;; concern themselves with one of the main three. First, the left partial-tree
;;; is processed, of the entire potential tree. Because this is a recursive
;;; procedure, the partial-tree can be called again and again. Once the partial-tree
;;; for the left side has been evaluated, the procedure assigns an entry for this
;;; current tree and then goes through the same process of evaluating a partial
;;; tree for the ride side.
;;;
;;; After all of this has been done, the entire tree is created with make-tree and
;;; joined with the remaining elements of the list to be placed inan outer tree
;;; structure, consisting of the right side and entry. These steps are taken all
;;; over again until all of the elements initially given have been placed in the
;;; tree structure.

;;;     5
;;;  /    \
;;; 1      9
;;;  \     /\
;;;   3   7  11

;;; b.
;;; A computation is done on every element of the list given, allowing the procedure
;;; to run at O(n) time.
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

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))