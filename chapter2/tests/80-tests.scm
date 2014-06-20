#lang racket
(require rackunit rackunit/text-ui)
(require "../80.scm")

(define sicp-2.80-tests
  (test-suite
    "Tests for SICP exercise 2.80"

    (check-equal? (=zero? (make-scheme-number 0)) #t)
    (check-equal? (=zero? (make-rational 0 3)) #t)
    (check-equal? (=zero? (make-complex-from-mag-ang 0 0)) #t)
    (check-equal? (=zero? (make-complex-from-real-imag 0 0)) #t)))
(run-tests sicp-2.80-tests)
