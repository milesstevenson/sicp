(load "../../test-manager/load.scm")
(load "../35.scm")

(in-test-group
 sicp-2.35-tests

 (define-test (standard-tests)
   (check (equal? 5 (count-leaves '(1 2 (3 4) 5))))))
(run-registered-tests)
(run-test '(sicp-2.35-tests))