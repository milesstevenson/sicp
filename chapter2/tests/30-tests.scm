(load "../../test-manager/load.scm")
(load "../30.scm")

(in-test-group
 sicp-2.30-tests

 (define-test (square-tree-tests)
   (check (equal? (square-tree '(1 2 3 (4 5))) (square-tree2 '(1 2 3 (4 5)))))
   (check (equal? (square-tree '(1 2 (9 0) (4 5))) (square-tree2 '(1 2 (9 0) (4 5)))))
   (check (equal? (square-tree '(1 -30 -20 (95))) (square-tree2 '(1 -30 -20 (95)))))))
(run-registered-tests)
(run-test '(sicp-2.30-tests))