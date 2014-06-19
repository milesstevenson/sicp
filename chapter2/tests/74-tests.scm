#lang racket
(require rackunit rackunit/text-ui)
(require "../74.scm")

(define sicp-2.74-tests
  (test-suite
    "Tests for SICP exercise 2.74"

    (check-equal? (get-record 'foo "bob") (find-employee-record "bob" joined-divisions))
))

(run-tests sicp-2.74-tests)