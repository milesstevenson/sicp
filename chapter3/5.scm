#lang racket        
(define (square x) (* x x))

(define (in-unit-circle? x y)
  (<= (+ (square x)
         (square y))
      (square 1)))
(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (experiment)
    (let ((x (random-in-range x1 x2))
          (y (random-in-range y1 y2)))
          (P x y)))
  (* 4 (monte-carlo trials experiment)))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

;;; thanks again skanev
(define (random-in-range low high)
  (+ (* (random) (- high low))
     low))