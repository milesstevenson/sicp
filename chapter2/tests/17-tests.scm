(load "../../test-manager/load.scm")
(load "../17.scm")

(in-test-group
 sicp-2.17-tests

 (define-test (last-pair-test)
   (check (equal? (list 5) (last-pair (list 3 4 5))))
   (check (equal? (list -345) (last-pair (list 233 44 -345))))))

(run-registered-tests)
(run-test '(sicp-2.17-tests))
   