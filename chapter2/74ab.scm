#lang racket
;;; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-17.html#%_thm_2.74

;;; misc =======================================================================
(define table (make-hash))
(define (put op type item)
  (hash-set! table (list op type) item))
(define (get op type)
  (hash-ref table (list op type)))

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

;;; divisions ==================================================================
(define bar
  '(((salary 10000000) "lewis")
    ((salary 2000) "dave")
    ((salary 50000) "lavelle")))
;;; ===
(define foo
  (cons '("bob" (salary 500))
        (cons '("eva" (salary 800.50))
              (cons '("alyssa" (salary 650)) '()))))

;;; Foo Package ================================================================
(define (install-foo-package)
  (define (get-record employee)
    (car (filter (lambda (x) (eq? (car x) employee))
            foo)))
  (define (get-salary employee)
    (cdr (get-record employee)))
  (put 'get-record 'foo 
       (lambda (x) (attach-tag 'foo (get-record x))))
  (put 'get-salary 'foo
       (lambda (x) (attach-tag 'foo (get-salary x))))
  'foo-package-installed!)

;;; Bar Package ================================================================
(define (install-bar-package)
  (define (get-record employee)
    (car (filter (lambda (x) (eq? (cadr x) employee))
            bar)))
  (define (get-salary employee)
    (car (get-record employee)))
  (put 'get-record 'bar
       (lambda (x) (attach-tag 'bar (get-record x))))
  (put 'get-salary 'bar
       (lambda (x) (attach-tag 'bar (get-salary x))))
  'bar-package-installed!)

;;; generic procedures ==========================================================
;;; a.
;;; Here the only information to be supplied by the user is the file or branch that
;;; the employee works under, and the employee's name. Get-record is a generic
;;; operation that will use data-directed programming, searching the table for
;;; the type of file provided and executing get-record of type file on the
;;; employee's name.
(define (get-record division employee) ((get 'get-record division) employee))

;;; b.
;;; This was done very similarly to part a. Record can be structured without each
;;; element being tagged. When the value is found, though, it is tagged incase other
;;; generic operations are to use it, to check for its type.
;;;
;;; This could have been written in a number of ways, and my way probably isn't
;;; ideal, I'm sure.
(define (get-salary division employee) ((get 'get-salary division) employee))

;;; install packages ===========================================================
(install-foo-package)
(install-bar-package)