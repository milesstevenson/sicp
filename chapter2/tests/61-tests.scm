#lang racket
(require rackunit rackunit/text-ui)
(require "../61.scm")

(define sicp-2.61-tests
  (test-suite
   "Tests for SICP exercise 2.61"
   (check-equal? '(1 2 3 9) (adjoin-set 1 '(2 3 9)))
   (check-equal? '(1 3 5 7) (adjoin-set 5 '(1 3 7)))
   (check-equal? '(1 3) (adjoin-set 3 '(1 3)))))
(run-tests sicp-2.61-tests)