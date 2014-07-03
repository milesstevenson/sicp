#lang racket
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
(provide =zero?
         make-complex-from-real-imag
         make-complex-from-mag-ang
         make-integer
         make-rational)

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
             (apply coerced-proc (map contents coerced-args)))))))
             
             


(define (square x) (* x x))

;;; generic arithmetic =========================================================

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

;;; ordinary numbers ===========================================================

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
       
  "Integer number package installed!")

(define (make-integer n)
  ((get 'make 'integer) n))
;;; rational numbers ===========================================================

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
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
  (put 'equ? '(real real)
       (lambda (x y) (equal? x y)))
  (put '=zero? '(real)
       (lambda (x)
         (= 0 x)))
  (put 'make 'real
       (lambda (x) (tag x)))
  (put 'raise '(real)
       (lambda (x) (cons 'complex
                         (cons 'rectangular 
                               (cons x 0)))))
  (put 'level 'real
       3)
       
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
    (sqrt (+ (square (real-part z)) (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  
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
  'done)

;;; polar package ==============================================================
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  
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
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  (define (add-complex-to-schemenum z x)
    (make-from-real-imag (+ (real-part z) x)
                         (imag-part z)))
  
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex integer)
     (lambda (z x) (tag (add-complex-to-schemenum z x))))
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
  
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'equ? '(complex complex)
       (lambda (x y) (and (equal? (magnitude x) (magnitude y))
                          (equal? (angle x) (angle y)))))
  (put '=zero? '(complex)
       (lambda (x) (and (= (real-part x) 0)
                        (= (imag-part x) 0))))
  (put 'level 'complex
       4)
  
  "Complex number package installed!")

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;;; generic procedures for complex ==============================
(define (real-part z)
  (apply-generic 'real-part z))
(define (imag-partt z)
  (apply-generic 'imag-partt z))
(define (magnitude z)
  (apply-generic 'magnitude z))
(define (angle z)
  (apply-generic 'angle z))

(define (integer->complex n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'integer 'complex integer->complex)

;;; general generic procedures =================================================
(define (equ? num1 num2)
  (apply-generic 'equ? num1 num2))
(define (=zero? x)
  (apply-generic '=zero? x))
(define (raise type)
  (apply-generic 'raise type))
;;; load up the packages =======================================================
(install-real-package)
(install-integer-package)
(install-rational-package)
(install-polar-package)
(install-rectangular-package)
(install-complex-package)