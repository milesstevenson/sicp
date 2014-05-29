(load "../../test-manager/load.scm")
(load "../36.scm")

(in-test-group
 sicp-2.36-tests

 (define-test (standard-tests)
   (check (equal? '(22 26 30) (accumulate-n + 0 a)))))
(run-registered-tests)
(run-test '(sicp-2.36-tests))
