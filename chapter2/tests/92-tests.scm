#lang racket
(require rackunit rackunit/text-ui)
(require "../92.scm")

(define sicp-2.92-tests
  (test-suite
    "Tests for SICP exercise 2.92"

    (check-equal? (mul (make-poly 'x '((1 1) (0 2)))
                       (make-poly 'y '((1 1) (0 2))))
                  '(polynomial x (1 (polynomial y (1 (number . 1)) (0 (number . 2)))) (0 (polynomial y (1 (number . 2)) (0 (number . 4))))))
    (check-equal? (add (make-poly 'x '((1 1) (0 2)))
                       (make-poly 'y '((1 1) (0 2))))
                  '(polynomial x (1 1) (0 (polynomial y (1 1) (0 (number . 4))))))
    (check-equal? (add (make-poly 'x (list (list 0 (make-poly 'y '((1 1) (0 2))))))
                       (make-poly 'y '((1 1) (0 2))))
                  '(polynomial y (1 1) (0 (polynomial x (0 (polynomial y (1 1) (0 (number . 4))))))))))

(run-tests sicp-2.92-tests)