#lang racket
(require rackunit rackunit/text-ui)
(require "../2.scm")

(define s (make-monitored sqrt))
(define t (make-monitored (lambda (x) (* x x))))


(define sicp-3.02-tests
  (test-suite
   "Tests for SICP exercise 3.02"
   (check-equal? (s 100) 10)
   (check-equal? (s 'how-many-calls?) 1)
   (check-equal? (s 81) 9)
   (check-equal? (s 'how-many-calls?) 2)
   (check-equal? (t 5) 25)
   (check-equal? (t 'how-many-calls?) 1)))

(run-tests sicp-3.02-tests)