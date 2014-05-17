(load "../../test-manager/load.scm")
(load "../34.scm")

(in-test-group
 sicp-2.34-tests

 (define-test (standard-tests)
   (check (equal? 63 (horner-eval 2 '(1 3 4 5))))
   (check (equal? 87 (horner-eval 2 '(1 15 22 0 -2))))
   (check (equal? 2 (horner-eval 2 '(2))))))
(run-registered-tests)
(run-test '(sicp-2.34-tests))