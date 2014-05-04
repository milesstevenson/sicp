(load "../../test-manager/load.scm")
(load "../19.scm")

(in-test-group
 sicp-2.19-tests

 (define-test (last-pair-test)
   (check (equal? 2 (count-change 5)))
   (check (equal? 292 (count-change 100)))))

(run-registered-tests)
(run-test '(sicp-2.19-tests))