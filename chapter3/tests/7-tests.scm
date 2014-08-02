#lang racket
(require rackunit rackunit/text-ui)
(require "../7.scm")

(define A (make-account 100 'pass))
(define B (make-joint A 'pass 'word))

(define sicp-3.07-tests
  (test-suite
   "Tests for SICP exercise 3.07"
   (check-equal? ((B 'word 'withdraw) 20) 80)
   (check-equal? ((A 'pass 'deposit) 10) 90)
   (check-equal? ((B 'word 'withdraw) 20) 70)))
   

(run-tests sicp-3.07-tests)