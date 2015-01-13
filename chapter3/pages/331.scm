#lang racket
(define (cons x y)
  (let ((new (get-new-par)))
    (set-car! new x)
    (set-cdr! new y)
    new))

;;; When compoud data was introduces, it was observed that pairs can be represented
;;; purely in terms of procedures
(define (cons x y)
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          (else (error "Undefined operation -- CONS" m))))
  dispatch)

(define (car z) (z 'car))
(define (cdr z) (z 'cdr))

;;; The same observation is true for mutable data. Mutable data objects can be
;;; implemented as procedures using assignment and local state.
(define (cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else (error "Undefined operation -- CONS" m))))
  dispatch)

(define (set-car! z new-value)
  ((z 'set-car!) new-value))
(define (set-cdr! z new-value)
  ((z 'set-cdr!) new-value))