(load "../../test-manager/load.scm")
(load "../5.scm")

(in-test-group
 sicp-2.05-tests
 
 (define-test (car23-test)
   "Checking that car23 returns a of (cons a b)"
   (check (= 0 (car23 (cons23 0 0))) "car23 should return as 0")
   (check (= 1 (car23 (cons23 1 2))) "car23 should return as 1")
   (check (= 10 (car23 (cons23 10 8))) "car23 should return as 10"))

 (define-test (cdr23-test)
   "Checking that car23 returns a of (cons a b)"
   (check (= 0 (cdr23 (cons23 0 0))) "cdr23 should return as 1")
   (check (= 2 (cdr23 (cons23 1 2))) "cdr23 should return as 2")
   (check (= 8 (cdr23 (cons23 10 8))) "cdr23 should return as 8")))

(run-registered-tests)
(run-test '(sicp-2.05-tests))