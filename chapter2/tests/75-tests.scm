#lang racket
(require rackunit rackunit/text-ui)
(require "../75.scm")

(define sicp-2.75-tests
  (test-suite
   "Tests for SICP exercise 2.75"
   
   (check-equal? ((make-from-real-imag 3 4) 'magnitude) 5)
   (check-equal? ((make-from-real-imag 3 4) 'angle) 0.9272952180016122)
   (check-equal? ((make-from-real-imag 3 4) 'real-part) 3)
   (check-equal? ((make-from-real-imag 3 4) 'imag-part) 4)))

(run-tests sicp-2.75-tests)
   
   