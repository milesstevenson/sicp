(load "../../test-manager/load.scm")
(load "../20.scm")

(in-test-group
 sicp-2.20-tests

 (define-test (same-parity-test)
   (check (equal? (list 1 3 5 7) (same-parity 1 2 3 4 5 6 7 8)))
   (check (equal? (list 2 4 6) (same-parity 2 3 4 5 6 7)))))

(run-registered-tests)
(run-test '(sicp-2.20-tests))