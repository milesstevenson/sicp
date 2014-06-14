#lang racket
(require rackunit rackunit/text-ui)
(require "../68.scm")

(define sicp-2.68-tests
  (test-suite
   "Tests for SICP exercise 2.68"
   (check-equal? sample-message (encode (decode sample-message sample-tree) sample-tree))))

(run-tests sicp-2.68-tests)