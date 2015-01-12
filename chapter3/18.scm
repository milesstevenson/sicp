;;; Write a procedure that examines a list and determines whether it contains
;;; a cycle, that is, whether a program that tried to find the end of the list
;;; by taking successive cdrs would go into an infinite loop.
;;;
;;; The idea for solving this is much the same as 3.17. An auxillary data struct
;;; is kept on the initial list and successive cdrs of the list.

#lang racket
(provide (all-defined-out))
(require r5rs/init)



(define (cycle? x)
  (let ((cycle-check '()))
    (define (cycle-helper z)
      (cond ((null? z) #f)
            ((memq z cycle-check) #t)
            (else (set! cycle-check (cons z cycle-check))
                  (cycle-helper (cdr z)))))
    (cycle-helper x)))

(define (lastpair x)
  (if (null? (cdr x))
      x
      (lastpair (cdr x))))

(define (make-cycle x)
  (set-cdr! (lastpair x) x)
  x)
  