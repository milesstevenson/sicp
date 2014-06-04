#lang racket
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define (flip-horizontal painter)
  ((transform-painter (make-vect 1.0 0.0)
                      (make-vect 0.0 0.0)
                      (make-vect 1.0 1.0))
   painter))

(define (rotate-180 painter)
  ((transform-painter (make-vect 1 1)
                      (make-vect 0 1)
                      (make-vect 1 0))
   painter))

(define (rotate-270 painter)
  ((transform-painter (make-vect 0 1)
                       (make-vect 0 0)
                       (make-vect 1 1))
   painter))