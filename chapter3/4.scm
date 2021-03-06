#lang racket
(provide (all-defined-out))

(define (make-account balance password)
  (let ((count 0)) 
    (define (call-the-cops x) "The cops have been called!!")
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (dispatch m1 m2)
      (cond ((and (eq? m1 password) (eq? m2 'withdraw))
             withdraw)
            ((and (eq? m1 password) (eq? m2 'deposit))
             deposit)
            ((not (eq? m1 password))
             (begin (set! count (+ count 1))
                    (if (> count 7)
                        call-the-cops
                        (error "Incorrect password"))))
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         (list m1 m2)))))
    dispatch))