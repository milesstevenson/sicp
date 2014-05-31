#lang planet neil/sicp

(define (split outer inner)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split outer inner) painter (- n 1))))
          (outer painter (inner smaller smaller))))))
      