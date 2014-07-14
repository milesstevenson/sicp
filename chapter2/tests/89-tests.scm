
#lang racket
(require rackunit rackunit/text-ui)
(require "../89.scm")

(define sicp-2.89-tests
  (test-suite
    "Tests for SICP exercise 2.89"

    (check-equal? (make-dense-polynomial 'x (list (make-integer 5) (make-real 4.3) (make-integer 3)))
                  '(dense x (integer . 5) (real . 4.3) (integer . 3)))
    (check-equal? (negate (make-dense-polynomial 'x (list (make-integer 5) (make-real 4.3) (make-integer 3))))
                  '(dense x (integer . -5) (real . -4.3) (integer . -3)))
    (check-equal? (=zero? (make-dense-polynomial 'x (list (make-integer 5) (make-real 4.3) (make-integer 3))))
                  #f)
    (check-equal? (=zero? (make-dense-polynomial 'x (list (make-integer 0) (make-real 0) (make-integer 0))))
                  #t)))

(run-tests sicp-2.89-tests)
