(load "../../test-manager/load.scm")
(load "../27.scm")

(in-test-group
 sicp-2.27-tests

 (define-test (same-parity-test)
   (check (equal? (list (list 4 3) (list 2 1)) (reverse-deep (list (list 1 2) (list 3 4)))))
   (check (equal? (list (list 7 6 4 3) -34 4 3 (list 14)) (reverse-deep (list (list 14)
                                                                   3 4 -34
                                                                   (list 3 4 6 7)))))))
(run-registered-tests)
(run-test '(sicp-2.27-tests))