#lang racket
(provide (all-defined-out))

(define (make-monitored f)
  (let ((count 0))
    (define (mf x)
      (if (eq? x 'how-many-calls?)
          count
          (begin (set! count (+ count 1))
                 (f x))))
    mf))