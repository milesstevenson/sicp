#lang racket
;;; Through DeMorgan's Law, we can see that (OR A B) is equivalent to 
;;; (NOT (NOT (OR A B))
;;; (NOT (AND (NOT A) (NOT B))
(require r5rs/init)
(require "utils.scm")
(define (or-gate-29 a b output)
  (let ((c (make-wire))
        (d (make-wire))
        (e (make-wire)))
    (inverter a c)
    (inverter b d)
    (and-gate c d e)
    (inverter e output))
  'ok)
    
;;; If we allow the inverter-delay to be 2 and the and-gate-delay to be 3
;;; we have 2 + 2 + 3 = 7 as the or-gate-delay. Although there are 3 inverters
;;; used, two of them run in parallel.