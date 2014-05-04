;;; The order of us/uk-coins does NOT matter.
;;;
;;; (cc 5 (1 5 10 25 50))
;;; |
;;; (cc 5  (5 10 25 50)) + (cc 4 (1 5 10 25 50))
;;; |                      |
;;; ...                    ...
;;;
;;; The coin list is cdred everytime the first recursive branch
;;; is entered, so using a different order of the coin list would
;;; give us different results. These results would STILL give the
;;; same answer, though. Let's take a reversed us-coin list as an
;;; example.
;;;
;;; Instead of the procedure checking if it's possible to make
;;; change from high-coin values first, each recursive branch (in the
;;; early stage of the process) would instead check for whether
;;; low-coin values can make change for the given amount first.
;;;
;;; I think that using a an ascending ordered list could reduce some
;;; run-time on this procedure.

(define us-coins (list 50 25 10 5 1))
(define us-coins2 (reverse us-coins))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (count-change amount)
  (cc amount us-coins2))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))
  
(define (first-denomination coin-values)
  (car coin-values))

(define (no-more? items)
  (null? items))

(define (except-first-denomination coin-values)
  (cdr coin-values))