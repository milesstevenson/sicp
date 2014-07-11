#lang racket
(require rackunit rackunit/text-ui)
(require "../87.scm")

(define A (make-polynomial 'x (list (list 3 (make-complex-from-real-imag (make-rational 2 3)
                                                                         (make-integer 4)))
                                    (list 5 (make-integer 6)) (list 0 (make-integer 1)))))
(define B (make-polynomial 'x (list (list 5 (make-complex-from-real-imag 0 0))
                                    (list 3 (make-integer 0)) (list 0 (make-integer 0)))))
(define sicp-2.87-tests
  (test-suite
    "Tests for SICP exercise 2.87"

    (check-equal? (=zero? A) #f)
    (check-equal? (=zero? B) #t)))

(run-tests sicp-2.87-tests)