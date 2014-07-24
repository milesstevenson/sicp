#lang racket
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
  (cond ((number? datum) 'integer)
        ((pair? datum) (car datum))
        (else
         (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else
         (error "Bad tagged datum -- CONTENTS" datum))))

(define (generic-number? object)
  (let ((tag (type-tag object)))
    (if (or (equal? tag 'integer)
            (equal? tag 'rational)
            (equal? tag 'real)
            (equal? tag 'complex))
        true
        false)))

(define (drop object)
  (if (equal? (type-tag object) 'integer)
      object
      (let ((projected-obj (project object)))
        (let ((raised-obj (raise projected-obj)))
          (if (equal? (type-tag object) 'real)
              (let ((re-raised-obj (raise raised-obj)))
                (if (equ? re-raised-obj object)
                    (drop projected-obj)
                    object))
              (if (equ? raised-obj object)
                  (drop projected-obj)
                  object))))))

(define (apply-generic op . args)
  ;; Here's what was mentioned in having a more correct approach, to dealing with
  ;; apply-generic, than the naive implementation that the book recommended in 2.82:
  ;;
  ;; "Here we're trying to generalize apply-generic to handle any number of types. 
  ;; The trouble here will be that we do not have any previously saved data on the
  ;; types we're dealing with, regarding whether or not one is a sub-type or
  ;; super-type of the other. This would make things much easier and result in a
  ;; faster run-time after all the type relations were loaded once."
  ;;
  ;; It turns out that we now do indeed have a way to have saved data on the relation
  ;; between any types in these packages. We will be using our data-directed table
  ;; like so:
  ;;                                    OPERATIONS
  ;;
  ;;                     'raise                       'level
  ;;                + --------------------------------------------
  ;;                | (define raise             |
  ;;       integer  |   (lambda (x)             |       1           ...   
  ;;                |      (cons 'rational      |
  ;;                |            (cons x 1))))  |
  ;;                 ---------------------------------------------
  ;;       rational | (define raise             |
  ;;                |   (lambda (x)             |       2           ...
  ;;                |      ( . . .)))           |
  ;; TYPES           ---------------------------------------------
  ;;       real     | (define raise             |
  ;;                |   (lambda (x)             |       3           ...
  ;;                |      ( . . .)))           |
  ;;                 ---------------------------------------------
  ;;       complex  | (define raise             |
  ;;                |   (lambda (x)             |       4           ...
  ;;                |      ( . . .)))           |
  ;;                + --------------------------------------------
  ;;
  ;; We'll search for the argument that holds the type with the highest level,
  ;; from our table, in the list of arguments and convert the other arguments
  ;; to have this same type by using our raise procedure from exercise 2.83!
  
  (define (find-highest-type tags)
    ;; Find the highest level in our table, out of all the arguments provided.
    (if (null? tags) 
        0
        (max (get 'level (car tags))
             (find-highest-type (cdr tags)))))
  
  (define (coerce-to target-level remaining-args result)
    ;; Our coerce-to internal procedure here is altered slightly from 2.82 in
    ;; that as it first argumet it takes a target level which all arguments
    ;; should match. Not so coincidentally, each argument will now be coerced
    ;; as well!
    (cond ((null? remaining-args) result)
          ((> target-level (get 'level 
                                (type-tag (car remaining-args))))
           (coerce-to target-level 
                      (append (list (raise (car remaining-args)))
                              (cdr remaining-args))
                      result))
          (else
           (coerce-to target-level 
                      (cdr remaining-args) 
                      (append result 
                              (list (car remaining-args)))))))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
           (apply proc (map contents args))
           (let* ((target-level (find-highest-type type-tags))
                  (coerced-args (coerce-to target-level args '()))
                  (coerced-tags (map type-tag coerced-args))
                  (coerced-proc (get op coerced-tags)))
             (drop (apply coerced-proc (map contents coerced-args))))))))
            

;;; generic arithmetic =========================================================
;;;
;;; If complex numbers are to be handled with magnitude, angle, real, and 
;;; imaginary parts where all can be integers or rational numbers now, there needs to
;;; be the following generic procedures added in every class of numbers:
;;;
;;; SQUARE, for magnitude of rectangular complex number
;;; COSINE, for real-part of polar complex numbers
;;; SINE, for imag-part of polar complex numbers
;;; ARCTAN, for angle of rectangular complex numbers
;;; SQUARE-ROOT, for magnitude of rectangular complex numbers
;;;
;;; Because of these new representations for complex numbers, changes also needed
;;; to be made to how the system projects a number. The drop procedure did not need
;;; changes, though.

(define (equ? num1 num2)
  (apply-generic 'equ? num1 num2))
(define (=zero? x)
  (apply-generic '=zero? x))
(define (raise object)
  (apply-generic 'raise object))
(define (project object)
  (apply-generic 'project object))
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (square x) (apply-generic 'square x))
(define (cosine x) (apply-generic 'cosine x))
(define (sine x) (apply-generic 'sine x))
(define (arctan y x) (apply-generic 'arctan y x))
(define (square-root x) (apply-generic 'square-root x))
(define (negate x) (apply-generic 'negate x))


;;; integer numbers ===========================================================

(define (install-integer-package)
  (define (tag x) (attach-tag 'integer x))
  (put 'add '(integer integer) 
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer) 
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer) 
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer) 
       (lambda (x y) (tag (/ x y))))
  (put 'square '(integer)
       (lambda (x) (tag (* x x))))
  (put 'square-root '(integer)
       (lambda (x) (attach-tag 'real (sqrt x))))
  (put 'cosine '(integer)
       (lambda (x) (attach-tag 'real (cos x))))
  (put 'sine '(integer)
       (lambda (x) (attach-tag 'real (sin x))))
  (put 'arctan '(integer integer)
       (lambda (y x) (attach-tag 'real (atan y x))))
  (put 'equ? '(integer integer)
       (lambda (x y) (equal? x y)))
  (put '=zero? '(integer)
       (lambda (x)
         (= 0 x)))
  (put 'make 'integer
       (lambda (x) (tag x)))
  (put 'raise '(integer)
       (lambda (x) (cons 'rational 
                         (cons x 1))))
  (put 'level 'integer
       1)
  (put 'project '(integer)
       (lambda (x)
         (make-integer x)))
  (put 'negate '(integer)
       (lambda (x) (tag (- x))))
       
  "Integer number package installed!")

(define (make-integer n)
  ((get 'make 'integer) n))
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
  (put 'square '(rational)
       (lambda (x) (tag (mul-rat x x))))
  (put 'square-root '(rational)
       (lambda (x) (cons 'real (exact->inexact (sqrt (/ (numer x) (denom x)))))))
  (put 'cosine '(rational)
       (lambda (x) (cons 'real (cos (exact->inexact (/ (numer x) (denom x)))))))
  (put 'sine '(rational)
       (lambda (x) (cons 'real (sin (exact->inexact (/ (numer x) (denom x)))))))
  (put 'arctan '(rational rational)
       (lambda (y x) (cons 'real (atan (exact->inexact (/ (numer y) (denom y))) 
                                       (exact->inexact (/ (numer x) (denom x)))))))  
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
;;; real numbers ===============================================================

(define (install-real-package)
  (define (tag x) (attach-tag 'real x))
  (put 'add '(real real) 
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real) 
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real) 
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real) 
       (lambda (x y) (tag (/ x y))))
  (put 'square '(real)
       (lambda (x) (tag (* x x))))
  (put 'square-root '(real)
       (lambda (x) (tag (sqrt x))))
  (put 'cosine '(real)
       (lambda (x) (tag (cos x))))
  (put 'sine '(real)
       (lambda (x) (tag (sin x))))
  (put 'arctan '(real real)
       (lambda (y x) (tag (atan y x))))
  (put 'equ? '(real real)
       (lambda (x y) (equal? x y)))
  (put '=zero? '(real)
       (lambda (x)
         (= 0 x)))
  (put 'make 'real
       (lambda (x) (tag (exact->inexact x))))
  (put 'raise '(real)
       (lambda (x) (make-complex-from-real-imag x 0)))
  (put 'level 'real
       3)
  (put 'project '(real)
       (lambda (x)
         (cons 'integer (round x))))
  (put 'negate '(real)
       (lambda (x) (tag (- x))))
       
  "Real number package installed!")
(define (make-real n)
  ((get 'make 'real) n))

;;; rect package ===============================================================
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (square-root (add (square (real-part z)) (square (imag-part z)))))
  (define (angle z)
    (arctan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (mul r (cosine a)) (mul r (sine a))))
  
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-and 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'negate '(rectangular) 
       (lambda (x) (tag (make-from-real-imag (negate (real-part x))
                                             (negate (imag-part x))))))
  'done)

;;; polar package ==============================================================
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (mul (magnitude z) (cosine (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (sine (angle z))))
  (define (make-from-real-imag x y)
    (cons (square-root (add (square x) (square y)))
          (arctan y x)))
  
  ;; interface to the rest of system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'negate '(polar)
       (lambda (x) 
         (tag (negate (make-from-real-imag (real-part x) (imag-part x))))))
  'done)

;;; Complex numbers ============================================================
(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (magnitude z)
    (apply-generic 'magnitude z))
  (define (angle z)
    (apply-generic 'angle z))
  (define (real-part z)
    (apply-generic 'real-part z))
  (define (imag-part z)
    (apply-generic 'imag-part z))
  
  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
                         (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                       (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                       (sub (angle z1) (angle z2))))
  
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'negate '(complex)
       (lambda (x)
         (tag (negate x))))
  
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'equ? '(complex complex)
       (lambda (x y) (and (equal? (magnitude x) (magnitude y))
                          (equal? (angle x) (angle y)))))
  (put '=zero? '(complex)
       (lambda (x) (and (=zero? (real-part x))
                        (=zero? (imag-part x)))))
  (put 'level 'complex
       4)
  (put 'project '(complex)
       (lambda (x) 
         (cons 'real (if (pair? (real-part x))
                         (exact->inexact (contents (real-part x)))
                         (exact->inexact (real-part x))))))
                         
  
  "Complex number package installed!")


(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;;; generic procedures for complex ==============================
(define (real-part z)
  (apply-generic 'real-part z))
(define (imag-part z)
  (apply-generic 'imag-part z))
(define (magnitude z)
  (apply-generic 'magnitude z))
(define (angle z)
  (apply-generic 'angle z))


;;; Dense Polynomials ================================================================
(define (install-dense-polynomial-package)
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
  (define (adjoin-term term term-list)
    (if (generic-number? term)
        (cons term term-list)
        term-list))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term coeff) coeff)
  (define (order term-list)
    (- (length term-list) 1))
  (define (coeff term) term)
  (define (=poly-zero? term-list)
    (cond ((empty-termlist? term-list) #t)
          ((=zero? (coeff (first-term term-list)))
           (=poly-zero? (rest-terms term-list)))
          (else #f)))
  (define (poly-negate term-list)
    (if (empty-termlist? term-list)
        (the-empty-termlist)
        (cons (negate (first-term term-list))
              (poly-negate (rest-terms term-list)))))
  (define (dense->sparse p)
    (define (dense-termlist->sparse-termlist term-list)
      (cond ((empty-termlist? term-list)
             (the-empty-termlist))
            ((equal? (coeff term-list) 0)
             (dense-termlist->sparse-termlist (rest-terms term-list)))
            (else
             (cons (list (- (length term-list) 1) (coeff (first-term term-list)))
                   (dense-termlist->sparse-termlist (rest-terms term-list))))))
    (if (equal? (type-tag p) 'sparse)
        p
        (make-poly (variable p)
                   (dense-termlist->sparse-termlist (term-list p)))))  
  
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
            (let ((t1 (first-term L1)) (t2 (first-term L2)))
              (cond ((> (order L1) (order L2))
                     (adjoin-term t1 (add-terms (rest-terms L1) L2)))
                    ((< (order L1) (order L2))
                     (adjoin-term t2 (add-terms L1 (rest-terms L2))))
                    (else
                     (adjoin-term
                      (make-term (add (coeff t1) (coeff t2)))
                      (add-terms (rest-terms L1) (rest-terms L2)))))))))
  
  (define (add-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- ADD-POLY" (list p1 p2))))
  
  (define (sub-poly p1 p2)
    (add-poly p1 
         (make-poly (variable p2) 
                    (poly-negate (term-list p2)))))
  
  ;; interface to the rest of the system
  (define (tag p) (attach-tag 'dense p))
  (put 'make 'dense
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(dense)
       (lambda (p) (or (empty-termlist? (term-list p))
                            (=poly-zero? (term-list p)))))
  (put 'negate '(dense)
       (lambda (p) (tag (make-poly (variable p) (poly-negate (term-list p))))))
  (put 'dense->sparse 'dense
       (lambda (p)
         (if (equal? 'sparse (type-tag p))
             p
             (cons 'sparse (dense->sparse (contents p))))))
  (put 'add '(dense dense)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(dense dense)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  "Dense Poly Package Installed!")

;;; Sparse Polynomials ================================================================
(define (install-sparse-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  
  (define (sparse->dense p)
    (define (sparse-termlist->dense-termlist term-list prev-order)
      (cond ((empty-termlist? term-list) (the-empty-termlist))
            ((equal? (- prev-order (order (first-term term-list))) 1)
             (cons (coeff (first-term term-list))
                   (sparse-termlist->dense-termlist (rest-terms term-list)
                                                    (order (first-term term-list)))))
            (else
             (cons 0 (sparse-termlist->dense-termlist term-list (- prev-order 1))))))
        (make-poly (variable p) 
                   (sparse-termlist->dense-termlist (term-list p) 
                                                    (+ (order (first-term (term-list p))) 1))))
  
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
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- ADD-POLY" (list p1 p2))))
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
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                             (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))
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
        
  
  ;; interface to the rest of the system
  (define (tag p) (attach-tag 'sparse p))
  (put 'add '(sparse sparse) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(sparse sparse)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'div '(sparse sparse)
       (lambda (p1 p2) (tag (div-poly p1 p2))))
  (put 'make 'sparse
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(sparse)
       (lambda (p) (or (empty-termlist? (term-list p))
                       (=poly-zero? (term-list p)))))
  (put 'negate '(sparse)
       (lambda (p) (tag (make-poly (variable p) (poly-negate (term-list p))))))
  (put 'sub '(sparse sparse)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'sparse->dense 'sparse
       (lambda (p) 
         (if (equal? (type-tag p) 'dense)
                     p
                     (cons 'dense (sparse->dense (contents p))))))
  "Sparse Polynomial package installed!")


;;; Polynomials ===================================================================
;;; Here we will define a package for handling operations on both representations
;;; of polynomials, similar to how the complex package handles its two represen-
;;; tations. 
;;;
;;; Whenever mul/div needs to be applied to polynomials, they will be
;;; converted to dense polynomials before the execution takes place.
;;; Whenever add/sub needs to be applied to polynomials, they will be
;;; converted to sparse polynomials before execution takes place.
;;;
;;; This option was not chosen out of efficiency, but rather convenience. It is
;;; easier to add/subtract a dense polynomial than it is to multiply/divide with
;;; one. Likewise, multiplication and division will be much easier in sparse form
;;; than addition and subtraction in sparse form.
(define (install-polynomial-package)
  ;; imported procedures from dense and sparse packages
  (define (make-dense-poly var terms)
    ((get 'make 'dense) var terms))
  (define (make-sparse-poly var terms)
    ((get 'make 'sparse) var terms))
  (define (dense->sparse p)
    ((get 'dense->sparse 'dense) p))
  (define (sparse->dense p)
    ((get 'sparse->dense 'sparse) p))
  
  ;; internal procedures
  ;; some of that sweet, sweet dispatch on type
  ;; *gags*
  (define (add-poly p1 p2)
    (if (and (equal? 'dense (type-tag p1))
             (equal? 'dense (type-tag p2)))
        (add p1 p2)
        (add (sparse->dense p1) (sparse->dense p2))))
  (define (sub-poly p1 p2)
    (if (and (equal? 'dense (type-tag p1))
             (equal? 'dense (type-tag p2)))
        (sub p1 p2)
        (sub (sparse->dense p1) (sparse->dense p2))))
  (define (mul-poly p1 p2)
    (if (and (equal? 'sparse (type-tag p1))
             (equal? 'sparse (type-tag p2)))
        (mul p1 p2)
        (mul (dense->sparse p1) (dense->sparse p2))))
  (define (div-poly p1 p2)
    (if (and (equal? 'sparse (type-tag p1))
             (equal? 'sparse (type-tag p2)))
        (div p1 p2)
        (div (dense->sparse p1) (dense->sparse p2))))
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) (tag (div-poly p1 p2))))
  (put 'make-dense-polynomial 'polynomial
       (lambda (var terms) (tag (make-dense-poly var terms))))
  (put 'make-sparse-polynomial 'polynomial
       (lambda (var terms) (tag (make-sparse-poly var terms))))
  (put 'dense->sparse 'polynomial
       (lambda (p) (dense->sparse p)))
  (put 'sparse->dense 'polynomial
       (lambda (p) (sparse->dense p)))
  "Polynomial package installed!")

(define (make-dense-polynomial var terms)
  ((get 'make-dense-polynomial 'polynomial) var terms))
(define (make-sparse-polynomial var terms)
  ((get 'make-sparse-polynomial 'polynomial) var terms))
;;; load up the packages =======================================================

(install-integer-package)
(install-rational-package)
(install-real-package)
(install-polar-package)
(install-rectangular-package)
(install-complex-package)
(install-dense-polynomial-package)
(install-sparse-polynomial-package)
(install-polynomial-package)