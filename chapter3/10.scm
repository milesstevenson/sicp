#lang racket
(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

;;; If we desugar the let expression in make-withdraw, we have
;;; the following:

(define make-withdraw-desugared
  (lambda (initial-amount)
    ((lambda (balance)
       (lambda (amount)
         (if (>= balance amount)
             (begin (set! balance (- balance amount))
                    balance)
             "Insufficient funds"))) initial-amount)))

;;; 
;;;
;;; Taking a stab at this. I don't really know what I'm doing.
;;; Others were referenced such as: 
;;;
;;; https://github.com/skanev/playground/blob/master/scheme/sicp/03/10.scm
;;; http://wqzhang.wordpress.com/2009/07/13/sicp-exercise-3-10/
;;; https://github.com/bishboria/SICP/blob/master/exercises/3.10.lisp
;;; and #racket on Freenode IRC.
;;;
;;; Alright, here we go.
;;; (define W1 (make-withdraw 100))
;;; 

;     ____________________________________________________________________________________
;    |                                  global env                                        |
;    |   make-withdraw-desugared: -----------------------------------------------------   |
;    |   W1:+                                                                         |   |
;    |      |                                                                         |   |
;    |______|_________________________________________________________________________|___|
;           |              ^                                                          |   ^
;           |              |                                                         O O--|
;           |        ______|___________                                                   |
;           |   E1->|                  |                                                  |
;          O O----->|initial-amount:100|                              parameters: initial-amount
;          |        |_____________ ____|                              body: ((lambda (balance)
;          |                         ^                                           ...) initial-amount)
;          |                         |
;          |                         |
;         parameters: amount         |
;         body: (if (>= ...)         |
;                   ...)             |
;                       _____________|
;                 E2-> | balance: 100|
;                      |             |
;                       -------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (W1 50)
;;;
;     ____________________________________________________________________________________
;    |                                  global env                                        |
;    |   make-withdraw-desugared: -----------------------------------------------------   |
;    |   W1:+                                                                         |   |
;    |      |                                                                         |   |
;    |______|_________________________________________________________________________|___|
;           |              ^                                                          |   ^
;           |              |                                                         O O--|
;           |        ______|___________                                                   |
;           |   E1->|                  |                                                  |
;          O O----->|initial-amount:100|                              parameters: initial-amount
;          |        |_____________ ____|                              body: ((lambda (balance)
;          |                         ^                                           ...) initial-amount)
;          |                         |
;          |                         |
;         parameters: amount         |
;         body: (if (>= ...)         |
;                   ...)             |
;                       _____________|
;                 E2-> | balance: 100|
;                      |             |
;                       -------------
;                                 ^
;                                 |
;                       __________|__
;                 E3-> |amount: 50   |
;                      |             |
;                       -------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (define W2 (make-withdraw 100))
;     ____________________________________________________________________________________
;    |                                  global env                                        |
;    |   make-withdraw-desugared: -----------------------------------------------------   |
;    |   W1:+                                                                         |   |
;    |   W2:|----------------------------------                                       |   |
;    |______|_________________________________|_______________________________________|___|
;           |              ^                  |              ^                        |   ^
;           |              |                  |              |                       O O--|
;           |        ______|___________       |              |                            |
;           |   E1->|                  |      |              |                             |
;          O O----->|initial-amount:100|      |              |         parameters: initial-amount
;          |        |_____________ ____|      |              |         body: ((lambda (balance)
;          |                         ^        |            __|________________    ...) initial-amount)
;          |                         |        |      E3-> |                  |
;          |<-----------------------|--------O O--------->|initial-amount:100| 
;         parameters: amount         |                    |__________________|
;         body: (if (>= ...)         |                           ^
;                   ...)             |                           |
;                       _____________|                        ___|_________ 
;                 E2-> | balance: 50 |;                 E4-> | balance: 50 |
;                      |_____________|                       |_____________|
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
