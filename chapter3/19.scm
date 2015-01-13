;;; Redo exercise 3.18 using an algorithm that takes only a constant amount of
;;; space. (This requires a very clever idea.)
;;;
;;; This solution looks quite ugly, but I think the space is constant.
#lang racket
(require r5rs/init)
(define (fast-cycle? x)
  (define (cycle-helper turtoise hare)
    (if (null? hare)
        #f
        (begin
          (set! hare (cdr hare))
          (if (null? hare)
              #f
              (begin
                (set! hare (cdr hare))
                (set! turtoise (cdr turtoise))
                (if (eq? hare turtoise)
                    #t
                    (cycle-helper turtoise hare)))))))
  (cycle-helper x x))

(define (lastpair x)
  (if (null? (cdr x))
      x
      (lastpair (cdr x))))

(define (make-cycle x)
  (set-cdr! (lastpair x) x)
  x)
