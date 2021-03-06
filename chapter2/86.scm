#lang racket
(provide (all-defined-out))
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
    (cond ((null? remaining-args) drop result)
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

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (square x) (apply-generic 'square x))
(define (cosine x) (apply-generic 'cosine x))
(define (sine x) (apply-generic 'sine x))
(define (arctan y x) (apply-generic 'arctan y x))
(define (square-root x) (apply-generic 'square-root x))

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
  (put 'project '(complex)
       (lambda (x) 
         (cons 'real (exact->inexact (real-part x)))))
                         
  
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

;;; general generic procedures =================================================
(define (equ? num1 num2)
  (apply-generic 'equ? num1 num2))
(define (=zero? x)
  (apply-generic '=zero? x))
(define (raise object)
  (apply-generic 'raise object))
(define (project object)
  (apply-generic 'project object))
;;; load up the packages =======================================================

(install-real-package)
(install-integer-package)
(install-rational-package)
(install-polar-package)
(install-rectangular-package)
(install-complex-package)