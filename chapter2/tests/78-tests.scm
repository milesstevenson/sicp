#lang racket
(require rackunit rackunit/text-ui)
(require "../78.scm")

(define sicp-2.78-tests
  (test-suite
    "Tests for SICP exercise 2.78"

    (check-equal? (make-scheme-number 5) 5)
    (check-equal? (contents (make-scheme-number 5)) 5)
    (check-equal? 'scheme-number (type-tag (make-scheme-number 5)))
    (check-equal? 25 (mul (make-scheme-number 5) (make-scheme-number 5)))
    (check-equal? 10 (add (make-scheme-number 5) (make-scheme-number 5)))
    (check-equal? 1 (div (make-scheme-number 5) (make-scheme-number 5)))
    (check-equal? 0 (sub (make-scheme-number 5) (make-scheme-number 5)))
))

(run-tests sicp-2.78-tests)