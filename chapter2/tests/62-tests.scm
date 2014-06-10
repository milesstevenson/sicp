#lang racket
(require rackunit rackunit/text-ui)
(require "../62.scm")

(define sicp-2.62-tests
  (test-suite
   "Tests for SICP exercise 2.62"
   (check-equal? '(1 2 3) (union-set '(1 2) '(2 3)))
   (check-equal? '(3 5 7 8 10 13) (union-set '(5 7 8) '(3 5 10 13)))
   (check-equal? '() (union-set '() '()))))

(run-tests sicp-2.62-tests)