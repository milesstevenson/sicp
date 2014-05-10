(load "../../test-manager/load.scm")
(load "../29.scm")

(define a (make-mobile (make-branch 2
                                    (make-mobile (make-branch 2 5)
                                                 (make-branch 2 10)))
                       (make-branch 2 20)))

;;; This is an exercise that definitely needs testing. Learned that the hard way.
(in-test-group
 sicp-2.29-tests

 (define-test (part-a-tests)
   (check (equal? '(2 5) (left-branch (make-mobile (make-branch 2 5) (make-branch 20 22)))))
   (check (equal? (make-branch 2 (make-mobile (make-branch 2 5) (make-branch 2 10)))
                  (left-branch a)))
   (check (equal? '(2 20) (right-branch a)))
   (check (equal? '((2 5) (2 10)) (branch-structure (left-branch a)))))

 (define-test (part-b-tests)
   (check (equal? 35 (total-weight a)))
   (check (equal? 10 (total-weight (make-mobile (make-branch 2 -15) (make-branch 2 25))))))

 (define-test (part-c-tests)
   (check (equal? #f (balanced? a)))
   (check (equal? #t (balanced? b)))
   (check (equal? #t (balanced? c)))))

(run-registered-tests)
(run-test '(sicp-2.29-tests))