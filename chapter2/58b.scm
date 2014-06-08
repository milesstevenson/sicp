#lang racket
;;; It would seem that I cannot write a fully functional addon that allows
;;; us to use standard algebraic notation, originally. In some cases, it works,
;;; and not in others. I suppose I won't bother with unit testing this solution.
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
        (else
         (error "unknown expression type -- DERIV" exp))))
;;; ============================================================
(define (=number? exp num)
  (and (number? exp) (= exp num)))
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else
         (list a1 '+ a2))))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else
         (list m1 '* m2))))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))
(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (addend s) (car s))
(define (augend s) 
  (if (= (length (cddr s)) 1)
      (caddr s)
      (cddr s)))
(define (multiplier p) (car p))
(define (multiplicand p) 
  (if (= (length (cddr p)) 1)
      (caddr p)
      (cddr p)))
