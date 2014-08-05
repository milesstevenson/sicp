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

;;; WARNING: This model below isn't correct. It is just my progress so far.
;;;          Fixing everything when I return.
;;;
;;; Here is my incorrect interpretation of the newly used evaluation model, evaluating
;;; the following expression(s). This is very much the same object as in figure 3.7. To
;;; understand why this modeling below is wrong, I will need to desugar figure 3.7 as well.
;;;
;;; A proper solution will come after work today or tomorrow morning.
;;;
;;; (define W1 (make-withdraw 100))
;;; more to come...

;     ____________________________________________________________________________________
;    |                                  global env                                        |
;    |   make-withdraw-desugared: -----------------------------------------------------   |
;    |   W1:+                                                                         |   |
;    |      |                                                                         |   |
;    |______|_________________________________________________________________________|___|
;           |              ^                                                          |  ^
;           |              |                                                         O O--
;           |        ______|______                                                   |
;           |   E1->|             |                                                  |
;          O O----->| balance: 100|                                       parameters: initial-amount
;          |        |_____________|                                       body: ((lambda (balance)
;          |                                                                      ...) initial-amount)
;          |
;          |
;         parameters: balance
;         body: (lambda (amount)
;                   ...)