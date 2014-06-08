#lang racket
(require rackunit rackunit/text-ui)
(require "../59.scm")

(define sicp-2.59-tests
  (test-suite
   "Tests for SICP exercise 2.59"
   (check-equal? '(1 2 3) (union-set '(1 2) '(2 3)))
   (check-equal? '(a b c) (union-set '(a b c) '(a b c)))
   (check-equal? '() (union-set '() '()))))

(run-tests sicp-2.59-tests)