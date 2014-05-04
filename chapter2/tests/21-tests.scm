(load "../../test-manager/load.scm")
(load "../21.scm")

(in-test-group
 sicp-2.21-tests

 (define-test (same-parity-test)
   (check (equal? (list 1 4 9 16) (square-list1 (list 1 2 3 4))))
   (check (equal? (list 1 4 9 16) (square-list2 (list 1 2 3 4))))
   
   (check (equal? (list 49 64 100 0) (square-list1 (list 7 -8 10 0))))
   (check (equal? (list 49 64 100 0) (square-list2 (list 7 -8 10 0))))))
   

(run-registered-tests)
(run-test '(sicp-2.21-tests))