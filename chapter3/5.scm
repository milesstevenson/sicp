#lang racket
;;; For making a rectangle here, we can use much of the same ideas from early
;;; chapter 2. Exercise 2.3, to be specific. We will use rectangles as objects
;;; with message passing, instead.
;;; ============================================================================
(define (make-rect x1 x2 y1 y2) 
  (define length 
      (- y2 y1))
    (define width
      (- x2 x1))
    (define area
      (* width length))
  (define (dispatch m)
    (cond ((eq? m 'area) area)
          ((eq? m 'length) length)
          ((eq? m 'width) width)
          (else (error "Unknown request -- MAKE-RECT" m))))
  dispatch)
;;; ============================================================================'

        
(define (square x) (* x x))

(define (in-unit-circle? x1 x2 y1 y2)
    ;; Again, we're assuming we're dealing with unit circles, and the book wanted.
  (<= (+ (square (- (random-in-range x1 x2) 0))
         (square (- (random-in-range y1 y2) 0)))
      (square 1)))
(define (estimate-integral P x1 x2 y1 y2 trials)
  ;; estimate-integral produces an estimate of pi by measuring the area of the
  ;; unit circle.
  (* ((make-rect x1 x2 y1 y2) 'area) (monte-carlo trials P)))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))