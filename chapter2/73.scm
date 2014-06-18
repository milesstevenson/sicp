#lang racket
(provide deriv)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A.
;;; The derivative procedure was changed to no longer dispatch on expression,
;;; but to rather modularize the procedure more by using data directed programming.
;;; The 'else' construct within 'cond' is the meat and potatoes of the procedure.
;;; It uses 'get' to retrieve the necessary operation to use, based on the type of
;;; expression it is currently dealing with.
;;;
;;; I suppose it would be possible to assimilate 'number?' and 'same-variable?' into
;;; the data-directed dispatch, but because the tag checked for is the operator and
;;; neither of those expresions have one, there would be complications. It's just
;;; much more smoother, with it done as the book has presented.

;;; B.
;;; done below

;;; C.
;;; done below

;;; D.
;;; swap the type and operator in 'put' down below in the deriv-package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num)
  (and (number? exp) (= exp num)))
;;;=============================================================================

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types -- APPLY-GENERIC"
           (list op type-tags))))))
;;; Thank you to github user skanev for his upload of exercises!
;;; https://github.com/skanev/playground/tree/master/scheme/sicp
;;; The table that we assumed is built-in. Don't peek.
(define table (make-hash))
(define (put op type item)
  (hash-set! table (list op type) item))
(define (get op type)
  (hash-ref table (list op type)))
;;;=============================================================================
;;; packages
  
;;; sloppy
(define (install-deriv-package)
  ;; internal procedures
  (define (addend s) (car s))
  (define (augend s)
    (cadr s))
  (define (multiplier p) (car p))
  (define (multiplicand p)
    (cadr p))
  (define (base e) (car e))
  (define (exponent e) (cadr e))
  
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  
  (define (make-exponentiation base exp)
    (cond ((=number? exp 0) 1)
        ((=number? exp 1) base)
        ((and (number? exp) (number? base))
         (expt base exp)) (else (list '** base exp))))
  
  (define (deriv-sum exp var)
        (make-sum (deriv (addend exp) var)
                  (deriv (augend exp) var)))
  (define (deriv-product exp var)
    (make-sum
     (make-product (multiplier exp)
                   (deriv (multiplicand exp) var))
     (make-product (deriv (multiplier exp) var)
                   (multiplicand exp))))
  (define (deriv-exponentiation exp var)
    (make-product
          (make-product (exponent exp) (make-exponentiation
                                        (base exp)
                                        (- (exponent exp) 1)))
          (deriv (base exp) var)))
  ;; interface to the rest of the system
  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-product)
  (put 'deriv '** deriv-exponentiation)
  'done)
;;;=============================================================================
         
(install-deriv-package)         
(define (deriv exp var)  
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))