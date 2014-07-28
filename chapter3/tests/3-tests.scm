#lang racket
(require rackunit rackunit/text-ui)
(require "../3.scm")

(define acc (make-account 100 'secret-password))
(define acc2 (make-account 100 'secret-password))

(define sicp-3.03-tests
  (test-suite
   "Tests for SICP exercise 3.03"
   (check-equal? ((acc 'secret-password 'withdraw) 20) 80)
   (check-equal? ((acc2 'secret-password 'withdraw) 30) 70)
   (check-equal? ((acc 'secret-password 'deposit) 5) 85)
   (check-equal? ((acc2 'secret-password 'deposit) 10) 80)))
   

(run-tests sicp-3.03-tests)