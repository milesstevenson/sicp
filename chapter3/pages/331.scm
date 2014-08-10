#lang racket
(define (cons x y)
  (let ((new (get-new-par)))
    (set-car! new x)
    (set-cdr! new y)
    new))