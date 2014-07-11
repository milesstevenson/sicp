#lang racket
(require rackunit rackunit/text-ui)
(require "../88.scm")

(define A (make-polynomial 'x (list (list 3 (make-complex-from-real-imag (make-rational 2 3)
                                                                         (make-integer 4)))
                                    (list 5 (make-integer 6)) (list 0 (make-integer 1)))))
(define B (make-polynomial 'x (list (list 5 (make-complex-from-real-imag 0 0))
                                    (list 3 (make-integer 0)) (list 0 (make-integer 0)))))
(define sicp-2.88-tests
  (test-suite
    "Tests for SICP exercise 2.88"

    (check-equal? (negate (make-polynomial 'x (list (list 1 (make-integer 2)) (list 0 (make-integer 1)))))
                  '(polynomial x (1 (integer . -2)) (0 (integer . -1))))
    (check-equal? (negate B)
                  '(polynomial x (5 (complex rectangular (integer . 0) integer . 0)) (3 (integer . 0)) (0 (integer . 0))))))

(run-tests sicp-2.88-tests)
