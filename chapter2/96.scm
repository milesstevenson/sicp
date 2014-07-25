#lang racket
;;; The codebase has been GREATLY REDUCED to make things simpler on myself. Its
;;; been appealing iteratively adding layers on top of the system with each
;;; exercise, but much complexity follows from it -- 2.91 was ~750 lines.
;;;
;;; To understand the concepts taught here, I'm going to just be dealing with
;;; simple integer/real numbers and sparse polynomials. Now that this is solved
;;; with regular numbers and polynomials, it COULD CERTAINLY be re-written to 
;;; deal with every type of number. I think to save time, and possibly hairs in
;;; my head, I'll stick to this simple implementation.
;;;
(provide (all-defined-out))

(define table1 (make-hash))
(define (put op type item)
  (hash-set! table1 (list op type) item))
(define (get op type)
  (hash-ref table1 (list op type) false))

(define table2 (make-hash))
(define (put-coercion op type element)
  (dict-set! table2 (list op type) element))
(define (get-coercion op type)
  (dict-ref table2 (list op type) false))
;;; ============================================================================

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (cond ((number? datum) 'number)
        ((pair? datum) (car datum))
        (else
         (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else
         (error "Bad tagged datum -- CONTENTS" datum))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for these types - APPLY-GENERIC" (list op type-tags))))))
            

;;; generic arithmetic =========================================================

(define (equ? num1 num2)
  (apply-generic 'equ? num1 num2))
(define (=zero? x)
  (apply-generic '=zero? x))
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (negate x) (apply-generic 'negate x))
(define (exp x y) (apply-generic 'exp x y))
(define (greatest-common-divisor x y) 
  (apply-generic 'greatest-common-divisor x y))


;;; numbers ===========================================================

(define (install-number-package)
  (define (tag x) (attach-tag 'number x))
  (put 'add '(number number) 
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(number number) 
       (lambda (x y) (tag (- x y))))
  (put 'mul '(number number) 
       (lambda (x y) (tag (* x y))))
  (put 'div '(number number) 
       (lambda (x y) (tag (/ x y))))
  (put 'equ? '(number number)
       (lambda (x y) (equal? x y)))
  (put '=zero? '(number)
       (lambda (x)
         (= 0 x)))
  (put 'make 'number
       (lambda (x) (tag x)))
  (put 'negate '(number)
       (lambda (x) (tag (- x))))
  (put 'greatest-common-divisor '(number number)
       (lambda (x y) (tag (gcd x y))))
  (put 'exp '(number number)
       (lambda (x y) (tag (expt x y))))
       
  "number number package installed!")

(define (make-number n)
  ((get 'make 'number) n))
;;; rational numbers ===========================================================

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
      (cons n d))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (add (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational)
       (lambda (x y) (and (equal? (numer x) (numer y))
                          (equal? (denom x) (denom y)))))
  (put '=zero? '(rational)
       (lambda (x) (= (numer x) 0)))
  (put 'raise '(rational)
       (lambda (x) (cons 'real (exact->inexact (/ (numer x) (denom x))))))
  (put 'level 'rational
       2)
  (put 'project '(rational)
       (lambda (x) (cons 'integer (numer x))))
  (put 'negate '(rational)
       (lambda (x) (tag (make-rat (- (numer x))
                                  (denom x)))))
  
  
  "Rational number package installed!")

(define (make-rational n d)
  ((get 'make 'rational) n d))
;;; Polynomials ================================================================
(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  
  ;; representation of terms and term lists
  ;; <procedures adjoin-term ... coeff from text below>
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (=poly-zero? terms)
    (cond ((null? terms) #t)
          ((=zero? (coeff (first-term terms)))
           (=poly-zero? (rest-terms terms)))
          (else #f)))
  
  (define (poly-negate terms)
    (if (empty-termlist? terms)
        (the-empty-termlist)
        (cons (list (order (first-term terms))
                    (negate (coeff (first-term terms))))
              (poly-negate (rest-terms terms)))))
  
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
            (let ((t1 (first-term L1)) (t2 (first-term L2)))
              (cond ((> (order t1) (order t2))
                     (adjoin-term t1 (add-terms (rest-terms L1) L2)))
                    ((< (order t1) (order t2))
                     (adjoin-term t2 (add-terms L1 (rest-terms L2))))
                    (else
                     (adjoin-term
                      (make-term (order t1) (add (coeff t1) (coeff t2)))
                      (add-terms (rest-terms L1) (rest-terms L2)))))))))
  
  (define (add-poly p1 p2)
    (cond ((same-variable? (variable p1) (variable p2))
           (make-poly (variable p1)
                      (add-terms (term-list p1)
                                 (term-list p2))))
          ((not (same-variable? (variable p1) (variable p2)))
           (if (>= (order (first-term (term-list p1))) (order (first-term (term-list p2))))
               (add-poly p1 (make-poly (variable p1) (adjoin-term (make-term 0 (cons 'polynomial p2))
                                                                  (the-empty-termlist))))
               (add-poly p2 (make-poly (variable p2) (adjoin-term (make-term 0 (cons 'polynomial p1))
                                                                  (the-empty-termlist))))))
          (else
           (error "Polys not in same var -- ADD-POLY" (list p1 p2)))))
  ;; <procedures used by add-poly>
  (define (mul-terms L1 L2) 
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))
  
  (define (mul-poly p1 p2)
    (cond ((same-variable? (variable p1) (variable p2))
           (make-poly (variable p1)
                      (mul-terms (term-list p1)
                                 (term-list p2))))
          ((not (same-variable? (variable p1) (variable p2)))
           (if (>= (order (first-term (term-list p1))) (order (first-term (term-list p2))))
               (mul-poly p1 (make-poly (variable p1) (adjoin-term (make-term 0 (cons 'polynomial p2))
                                                                  (the-empty-termlist))))
               (mul-poly p2 (make-poly (variable p2) (adjoin-term (make-term 0 (cons 'polynomial p1))
                                                                  (the-empty-termlist))))))
          (else
           (error "Polys not in same var -- MUL-POLY"
                  (list p1 p2)))))
  (define (sub-poly p1 p2)
      (add-poly p1 (make-poly (variable p2) (poly-negate (term-list p2)))))
  
  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1)) (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2))))
                (let ((rest-of-result
                       (div-terms (add-terms L1 (poly-negate (mul-term-by-all-terms 
                                                         (make-term new-o new-c) L2)))
                                  L2)))
                  (adjoin-term (make-term new-o new-c)
                               rest-of-result)))))))
  
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (div-terms (term-list p1) (term-list p2))
        (error "Polynomials don't use the same variables" 
               (list (variable p1) (variable p2)))))
  
  (define (integerizing-factor term1 term2)
    (let ((o1 (order (first-term term1)))
          (o2 (order (first-term term2)))
          (c (coeff (first-term term2))))
      (exp c (+ 1 (- o1 o2)))))
  
  (define (pseudoremainder-terms term1 term2)
    (let ((t1 (integerizing-factor term1 term2)))
      (cadr (memq '() (div-terms 
                       (mul-term-by-all-terms (make-term 0 t1) term1) 
                       term2)))))
  (define (remove-common-factors termlist)
   (let ((gcd_coeff (apply gcd (map contents (map coeff termlist)))))
     (mul-term-by-all-terms (make-term 0 (/ 1 gcd_coeff)) termlist)))
  
  (define (gcd-terms a b)
    (if (empty-termlist? b)
        (remove-common-factors a)
        (gcd-terms b (pseudoremainder-terms a b))))
  
  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1) (gcd-terms (term-list p1) (term-list p2)))
        (error "Polynomials don't use the same variables" 
               (list (variable p1) (variable p2)))))
  
  ;; interface to the rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'add '(polynomial number)
       (lambda (p n) (tag (add-poly (make-poly (variable p) (adjoin-term (make-term 0 n)
                                                                         (the-empty-termlist)))
                                    p))))
  (put 'add '(number polynomial)
       (lambda (n p) (tag (add-poly (make-poly (variable p) (adjoin-term (make-term 0 n)
                                                                         (the-empty-termlist)))
                                    p))))
  (put 'mul '(number polynomial)
       (lambda (n p) (tag (mul-poly (make-poly (variable p) (adjoin-term (make-term 0 n)
                                                                         (the-empty-termlist)))
                                    p))))
  (put 'mul '(polynomial number)
       (lambda (p n) (tag (mul-poly (make-poly (variable p) (adjoin-term (make-term 0 n)
                                                                         (the-empty-termlist)))
                                    p))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) (tag (div-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(polynomial)
       (lambda (p) (or (empty-termlist? (term-list p))
                       (=poly-zero? (term-list p)))))
  (put 'negate '(polynomial)
       (lambda (p) (tag (make-poly (variable p) (poly-negate (term-list p))))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'greatest-common-divisor '(polynomial polynomial)
       (lambda (p1 p2) (tag (gcd-poly p1 p2))))
  "Polynomial package installed!")

(define (make-poly var terms)
  ((get 'make 'polynomial) var terms))
;;; load up the packages =======================================================

(install-number-package)
(install-polynomial-package)
(install-rational-package)

(define P1 (make-poly 'x '((2 1) (1 -2) (0 1))))
(define P2 (make-poly 'x '((2 11) (0 7))))
(define P3 (make-poly 'x '((1 13) (0 5))))

(define Q1 (mul P1 P2))
(define Q2 (mul P1 P3))
(greatest-common-divisor Q1 Q2)
  