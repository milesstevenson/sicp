#lang racket
;;; Thank goodness for the Racket debugger! No more mit-scheme!
;(provide (all-defined-out))
(provide addend augend deriv multiplier multiplicand)

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (make-product (exponent exp) (make-exponentiation
                                        (base exp)
                                        (- (exponent exp) 1)))
          (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))
;;; ============================================================
(define (expression? x)
  (or (sum? x) (product? x) (exponentiation? x)))

(define (multiplicand p)
  (if (not (null? (cdddr p)))
      (append (list '* (caddr p)) (cdddr p))
      (make-product (caddr p) 1)))
      
(define (augend s)
  (if (not (null? (cdddr s)))
      (append (list '+ (caddr s)) (cdddr s))
      (make-sum (caddr s) 0)))
;;; ============================================================
(define (make-exponentiation base exp)
  (cond ((=number? exp 0) 1)
        ((=number? exp 1) base)
        ((and (number? exp) (number? base))
         (expt base exp))
        (else (list '** base exp))))
(define (base e) (cadr e))
(define (exponent e) (caddr e))
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        (else
         (list '+ a1 a2))))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        (else
         (list '* m1 m2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (=number? exp num)
  (and (number? exp) (= exp num)))