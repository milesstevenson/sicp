(load "../../test-manager/load.scm")
(load "../18.scm")

(in-test-group
 sicp-2.18-tests

 (define-test (last-pair-test)
   (check (equal? (list 1 2 3) (reverse (list 3 2 1))))
   (check (equal? (list 1) (reverse (list 1))))
   (check (equal? '() (reverse '())))))

(run-registered-tests)
(run-test '(sicp-2.18-tests))
   