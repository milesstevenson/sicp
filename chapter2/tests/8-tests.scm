(load "../../test-manager/load.scm")
(load "../8.scm")

(in-test-group
 sicp-2.08-tests

 (define-test (sub-interval-test)
   "Make sure sub-interval is working properly"
   (check (= 10 (lower-bound (sub-interval (make-interval 20 60) (make-interval 10 20)))))
   (check (= 40 (upper-bound (sub-interval (make-interval 20 60) (make-interval 10 20)))))
   (check (= 0 (lower-bound (sub-interval (make-interval 5 20) (make-interval 5 10)))))
   (check (= 10 (upper-bound (sub-interval (make-interval 5 20) (make-interval 5 10)))))))

(run-registered-tests)
(run-test '(sicp-2.08-tests))

