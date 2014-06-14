#lang racket
(provide encode sample-message sample-tree decode)

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((eq? x (car set)) true)
        (else (element-of-set? x (cdr set)))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight-leaf x) (caddr x)) 
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree))) 

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree (make-leaf 'D 1)
                                    (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

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