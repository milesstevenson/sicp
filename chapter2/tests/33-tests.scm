(load "../../test-manager/load.scm")
(load "../33.scm")

(in-test-group
 sicp-2.33-tests

 (define-test (standard-tests)
   (check (equal? '(1 4 9) (map1 square '(1 2 3))))
   (check (equal? '(1 2 3 4 5) (append1 '(1 2) '(3 4 5))))
   (check (equal? 3 (length1 '(1 2 3))))))
 (run-registered-tests)
 (run-test '(sicp-2.33-tests))