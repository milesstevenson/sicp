;;; If the two algebraic forumulas taken are proved to be equivalent,
;;; it's implied that to get from 1/((1/R1) + (1/R2)) to R1R2/(R1+R2)
;;; R1/R1 and R2/R2 must be accepted as the value 1.

;;; 1/((1/R1)+(1/R2)) = (R1/R1)*1/((1/R1)+(1/R2))*(R2/R2)
;;;                   = R1R2/((R1R2/R1) + (R1R2/R2))
;;;                   = R1R2/(R1+R2)

;;; We've already observed that dividing an interval by itself leaves room
;;; for error, from 2.14.

(define A (make-center-percent 3.5 .05)) ;(3.675 . 3.325)
(define test-interval (div-interval A A)) ;(.9047619047619049 . 1.1052631578947367)

;;; So par2 is indeed the better program for giving answers with less error, as these
;;; implications aren't needed as much as in p1.http://library.readscheme.org/