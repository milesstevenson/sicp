#lang racket
(provide generate-huffman-tree make-code-tree make-leaf)

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))  

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))
;;; ============================================================================
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (successive-merge items)
  ;; Successfully merges the smallest-weight elements of the set until there is
  ;; only one element left, which is the desired Huffman tree.
  (define (merge-help leaf-set tree)
    (if (null? leaf-set)
        tree
        (merge-help (cdr leaf-set) 
                    (make-code-tree (car leaf-set)
                                    tree))))
  ;; Atleast two elements are assumed to be in the set of symbol-frequency pair
  (merge-help (cddr items) (make-code-tree (car items)
                                           (cadr items))))
      
(define (make-leaf-set pairs)
  ;; Takes a list of symbol-frequency pairs such as ((A 4) (B 2) (C 1) (D 1))
  ;; and constructs an initial ordered set of leaves, ready to be merged
  ;; according to the Huffman algorithm.
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)   ; symbol
                               (cadr pair)) ; frequency
                    (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))