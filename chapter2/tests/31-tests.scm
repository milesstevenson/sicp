(load "../../test-manager/load.scm")
(load "../31.scm")

;; using solution from 2.30 to help with testing
(define (square-tree2 tree)
  (map (lambda (sub-tree)
           (if (pair? sub-tree)
               (square-tree2 sub-tree)
               (square sub-tree)))
       tree))

(in-test-group
 sicp-2.31-tests 

 (define-test (square-tree-tests)
   (check (equal? (square-tree '(1 2 3 (4 5))) (square-tree2 '(1 2 3 (4 5)))))
   (check (equal? (square-tree '(1 2 (9 0) (4 5))) (square-tree2 '(1 2 (9 0) (4 5)))))
   (check (equal? (square-tree '(1 -30 -20 (95))) (square-tree2 '(1 -30 -20 (95)))))))
(run-registered-tests)
(run-test '(sicp-2.31-tests))