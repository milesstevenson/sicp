#lang racket
;;; The delay time of this or-gate is the delay time of 3 and-gate-delays and 3 
;;; or-gate-delays

(require r5rs/init)
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let* ((b1 (logical-not (get-signal (and-gate a1 a1))))
           (b2 (logical-not (get-signal (and-gate a2 a2))))
           (new-value (logical-not (get-signal (and-gate b1 b2)))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure))