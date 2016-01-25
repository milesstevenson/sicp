#lang racket
(require r5rs/init)
(require "utils.scm")

(define (logical-or x y)
  (if (and (= x 0) (= y 0))
      0
      1))

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
          (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'done)

(define in1 (make-wire))
(define in2 (make-wire))
(define out (make-wire))
(probe 'out out)
(or-gate in1 in2 out)
(set-signal! in1 1)
(propagate)