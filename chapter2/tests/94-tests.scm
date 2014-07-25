#lang racket
(require rackunit rackunit/text-ui)
(require "../94.scm")

(define sicp-2.94-tests
  (test-suite
    "Tests for SICP exercise 2.94"

    (check-equal? (greatest-common-divisor (make-sparse-polynomial 'x '((4 1) (3 -1) (2 -2) (1 2)))
                                           (make-sparse-polynomial 'x '((3 1) (1 -1))))
                  '(polynomial sparse x (2 (integer . -1)) (1 (integer . 1))))
    (check-equal? (greatest-common-divisor (make-dense-polynomial 'x '(1 -1 -2 2 0))
                                           (make-sparse-polynomial 'x '((3 1) (1 -1))))
                  '(polynomial sparse x (2 (integer . -1)) (1 (integer . 1)) (0 0)))))

(run-tests sicp-2.94-tests)