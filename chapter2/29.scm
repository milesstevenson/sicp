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


;;;b
(define (total-weight mobile)
  (+ (find-weights (left-branch mobile))
     (find-weights (right-branch mobile))))
(define (find-weights branch)
  (if (not (pair? (branch-structure branch)))
      (branch-structure branch)
      (total-weight (branch-structure branch))))

;;;c
(define (balanced? mobile)
  ;; There are four cases here. Maybe more but this is all I could
  ;; come up with.
  ;;
  ;; (1) Left and right branch of local mobile hold a weight
  ;;     as their structure.
  ;;     Solution: If the torque of both branches are equal,
  ;;               return true, else the mobile is not balanced.
  ;;
  ;; (2) Left branch holds a weight structure, but right
  ;;     branch does not -- holds a sub-mobile.
  ;;     Solution: If the torque of the left branch is equal
  ;;               to the length of the right branch, the local
  ;;               mobile is balanced. Re-enter balanced? of
  ;;               the right side sub-mobile.
  ;;
  ;; (3) Left branch does not hold a weight structure, but
  ;;     the right branch does.
  ;;     Solution: If the length of the left branch is equal
  ;;               to the torque of the right branch, the local
  ;;               mobile is balanced. Re-enter balanced? of
  ;;               the left side sub-mobile.
  ;;
  ;; (4) Neither the left or the right side branches has a 
  ;;     weight structure.
  ;;     Solution: If the length of the left branch is equal to
  ;;               the length of the right branch, return the boolean
  ;;               of whether the sub-mobile of the left branch and
  ;;               the sub-mobile of the right branch are balanced.
  (define weight? integer?)
  (define (torque branch)
    (* (branch-length branch) (branch-structure branch)))
  (cond ((and (weight? (branch-structure (left-branch mobile))) ; (1)
	      (weight? (branch-structure (right-branch mobile))))
	 (equal? (torque (left-branch mobile))
		 (torque (right-branch mobile))))
	((and (weight? (branch-structure (left-branch mobile))) ; (2)
	      (not (weight? (branch-structure (right-branch mobile)))))
	 (if (equal? (torque (left-branch mobile))
		     (branch-length (right-branch mobile)))
	     (balanced? (branch-structure (right-branch mobile)))
	     #f))
	((and (not (weight? (branch-structure (left-branch mobile)))) ; (3)
	      (weight? (branch-structure (right-branch mobile))))
	 (if (equal? (branch-length (left-branch mobile))
		     (torque (right-branch mobile)))
	     (balanced? (branch-structure (left-branch mobile)))
	     #f))
	((and (not (weight? (branch-structure (left-branch mobile)))) ; (4)
	      (not (weight? (branch-structure (right-branch mobile)))))
	 (if (equal? (branch-length (left-branch mobile))
		     (branch-length (right-branch mobile)))
	     (and (balanced? (branch-structure (left-branch mobile)))
		  (balanced? (branch-structure (right-branch mobile))))
	     #f))))
	      
;;;Example mobile
(define a (make-mobile (make-branch 2
                                    (make-mobile (make-branch 2 5)
                                                 (make-branch 2 10)))
                       (make-branch 2 20)))
;;;               *
;;;              / \
;;;          2  /   \ 2
;;;             *   20
;;;            / \ 
;;;         2 /   \ 2
;;;           5   10
;;;-------------------------------------------------

;;;Example mobile
(define b (make-mobile (make-branch 2 10) 
		       (make-branch 20 (make-mobile (make-branch 5 3)
						    (make-branch 3 5)))))
;;;               *
;;;              / \
;;;           2 /   \
;;;            10    *
;;;                 / \
;;;              5 /   \ 3
;;;                3    5
;;;--------------------------------------------------

;;;Example mobile
(define c (make-mobile (make-branch 5 (make-mobile (make-branch 10 12) (make-branch 12 10)))
		       (make-branch 5 (make-mobile (make-branch 2 10) (make-branch 10 2)))))
;;;
;;;                                   *
;;;                                /     \
;;;                             5 /       \ 5
;;;                              /         \
;;;                             /           \
;;;                            *             *
;;;                           / \           / \
;;;                      10  /   \ 12   2  /   \ 10
;;;                         /     \       /     \
;;;                        /       \     /       \
;;;                       12        10   10       2


      
;;; d
;;; Thankfully, the programs written for parts c and b would not need
;;; to be changed. The selectors would need to be re-implemented to
;;; account for the lack of depth of nested pairs from using cons
;;; instead of list in our constructors. After that, each program which
;;; uses the constructors and selectors should run fine.