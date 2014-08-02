#lang racket
;;; rand-update isn't provided in the book, so let's come up with a predictable number of sequences on
;;; our own. Once again, I owe the github user skanev for guidance in this. Seeing how he did it was a
;;; big help. My initial problems were in my misunderstanding of scope in Racket.
;;;
;;; These numbers are only pseudo random in that
;;; x1 = initial-value
;;; x2 = (rand-update x1)
;;; x3 = (rand-update x2)
;;; ...
;;; etc
;;; It's set up this way so that it can generate repeatable sequences! I this is what
;;; book was asking for. The descriptions to exercises can be vague.
 
(define rand
  (let ((x 1)) 
    (define (rand-update x)
      (modulo (* x 44534324
                 (modulo 943853032 4434556))
              124508473235))   
    (define (dispatch m)
      (cond ((eq? m 'generate)
             (set! x (rand-update x))
             x)
            ((eq? m 'reset)
             (lambda (new-value) (set! x new-value)))
            (else
              (error "Unknown request -- RAND" (list m)))))
    dispatch))