;;; SICP 2.29

(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))

;;;a
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (if (integer? (branch-length (cdr mobile)))
      (cdr mobile)
      (car (cdr mobile))))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (if (integer? (cdr branch))
      (cdr branch)
      (car (cdr branch))))


(define a (make-mobile (make-branch 2
                                    (make-mobile (make-branch 2 5)
                                                 (make-branch 2 10)))
                       (make-branch 2 20)))


;;;b
(define (total-weight mobile)
  (+ (find-weights (left-branch mobile))
     (find-weights (right-branch mobile))))
(define (find-weights branch)
  (if (not (pair? (branch-structure branch)))
      (branch-structure branch)
      (total-weight (branch-structure branch))))

;;;c

      
  