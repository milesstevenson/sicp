(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define a (fold-right / 1 (list 1 2 3)));3/2
(define b (fold-left / 1 (list 1 2 3)));1/6
(define c (fold-right list '() (list 1 2 3)));(1 (2 (3 ())))
(define d (fold-left list '() (list 1 2 3)));(((() 1) 2) 3)

;;; The property of addition or multiplication should hold
;;; so that fold-right and fold-left yield the same results.

(define y (fold-right * 1 (list 1 2 3)))
(define z (fold-left * 1 (list 1 2 3)))