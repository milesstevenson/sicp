(load "../../test-manager/load.scm")
(load "../28.scm")

(in-test-group
 sicp-2.28-tests

 (define-test (same-parity-test)
   (check (equal? (list 1 2 3 4) (fringe (list (list 1 2) (list 3 4)))))
   (check (equal? (list 14 3 4 -34 3 4 6 7) (fringe (list (list 14)
                                                                   3 4 -34
                                                                   (list 3 4 6 7)))))))
(run-registered-tests)
(run-test '(sicp-2.28-tests))