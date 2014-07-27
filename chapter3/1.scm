#lang racket
(provide (all-defined-out))

(define (make-accumulator value)
  (lambda (addend)
    (begin (set! value (+ value addend))
           value)))