;;; SICP 2.28
;;;
;;; Write a procedure fringe that takes as argument a tree
;;; (represented as a list) and returns a list whose elements
;;; are all leaves of the tree arranged in left-to-right order.
;;; For example:
;;;
;;; (define x (list (list 1 2) (list 3 4)))
;;;
;;; (fringe x)
;;; ( 1 2 3 4)
;;;
;;; (fringe (list x x))
;;; (1 2 3 4 1 2 3 4)

(define (fringe items)
  (define (fringe-begin items)
    (define (fringe-deep-iter mark)
      (cond ((equal? mark -1) '())
            ((pair? (list-ref items mark))
             (append
              (fringe-begin (list-ref items mark))
              (fringe-deep-iter (- mark 1))))
            (else
             (append (list (list-ref items mark))
                     (fringe-deep-iter (- mark 1))))))
    (fringe-deep-iter (- (length items) 1)))
  (reverse (fringe-begin items)))