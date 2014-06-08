#lang racket
(require rackunit rackunit/text-ui)
(require "../60.scm")

(define sicp-2.60-tests
  (test-suite
   "Tests for SICP exercise 2.560"
   (check-equal? '(2 1 2 3) (union-set '(1 2) '(2 3)))
   (check-equal? '(a b c) (intersection-set '(a b c d) '(a b c)))
   (check-equal? true (element-of-set? 3 '(3 4)))
   (check-equal? '(1 1 2 3) (adjoin-set 1 '(1 2 3)))))
(run-tests sicp-2.60-tests)