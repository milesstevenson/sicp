#lang racket
;;; Encoding the message outputs a binary string of 87 digits.
;;; If we were to encode this song with a fixed-length code for the eight symbol
;;; alphabet, it would require log_2(8)*36 bits. That's 108 digits.

(provide generate-huffman-tree make-code-tree make-leaf)

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))  

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((eq? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

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

(define alphabet '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3)
                       (YIP 9) (WAH 1)))
(define lyrics '(GET A JOB
                     SHA NA NA NA NA NA NA NA NA
                     GET A JOB
                     SHA NA NA NA NA NA NA NA NA
                     WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
                     SHA BOOM))

(define (encode-symbol symbol tree)
  ;; Encode-symbl is a procedure that returns the list of bits that encodes a
  ;; given symbol according to a given tree.
  (let ((left-tree (left-branch tree))
        (right-tree (right-branch tree)))
    (cond ((element-of-set? symbol (symbols left-tree))
           (if (leaf? left-tree)
               (cons 0 '())
               (cons 0 (encode-symbol symbol left-tree))))
          ((element-of-set? symbol (symbols right-tree))
           (if (leaf? right-tree)
               (cons 1 '())
               (cons 1 (encode-symbol symbol right-tree))))
          (else '()))))
      
(define (encode message tree)
  ;; The encode procedure takes as arguments a massage and a tree and produces
  ;; the list of bits that gives the encoded message
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define message (generate-huffman-tree alphabet))