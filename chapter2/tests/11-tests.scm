(load "../../test-manager/load.scm")
(load "../11.scm")

(in-test-group
 sicp-2.11-tests

 (define-test (mul-test)
   "Check if our new interval multiplication procedure works as intended"
   (check (equal? (make-interval 3 8) (mul-interval (make-interval 1 2)
                                               (make-interval 3 4))))
   (check (equal? (make-interval -6 8) (mul-interval (make-interval 1 2)
                                                (make-interval -3 4))))
   (check (equal? (make-interval -8 -3) (mul-interval (make-interval 1 2)
                                                 (make-interval -4 -3))))
   (check (equal? (make-interval -4 8) (mul-interval (make-interval -1 2)
                                                (make-interval 3 4))))))
(run-registered-tests)
(run-test '(sicp-2.11-tests))