#lang racket
(require rackunit rackunit/text-ui)
(require "../1.scm")

(define A (make-accumulator 5))

(define sicp-3.01-tests
  (test-suite
   "Tests for SICP exercise 3.01"
   
   (check-equal? (A 10) 15)
   (check-equal? (A 10) 25)))

(run-tests sicp-3.01-tests)