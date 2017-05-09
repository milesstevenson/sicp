#lang racket
;;;; Works as intended, but requires two values out of a, b, and c to be set.
;;;; Perhaps this could be modified to additionally solve the constraint when
;;;; given only a, but that would require some additional logic to be added to
;;;; the adder constraint. Pretty fun either way.
(require r5rs/init)
(require "propagation.rkt")

(define (averager a b c)
  (let ((d (make-connector)) (e (make-connector)))
    (adder a b d)
    (constant 2 e)
    (multiplier e c d)
  ))
(define a (make-connector))
(define b (make-connector))
(define c (make-connector))

(averager a b c)
(probe "a value" a)
(probe "b value" b)
(probe "c value" c)
(set-value! c 5 'user)