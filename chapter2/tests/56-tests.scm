(load "../../test-manager/load.scm")
(load "../56.scm")

(in-test-group
 sicp-2.56-tests

 (define-test (equal?-test)
   "Make sure sub-interval is working properly"
   (check (equal? '(* 2 x) (deriv '(** x 2) 'x)))
   (check (equal? 1 (deriv '(** x 1) 'x)))
   (check (equal? 0 (deriv '(** x 0) 'x)))))

(run-registered-tests)
(run-test '(sicp-2.56-tests))

