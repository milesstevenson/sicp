(load "../../test-manager/load.scm")
(load "../54.scm")

(in-test-group
 sicp-2.54-tests

 (define-test (equal?-test)
   "Make sure sub-interval is working properly"
   (check (equal? (equal? 'a 'a) (myequal? 'a 'a)))
   (check (equal? (equal? '() '()) (myequal? '() '())))
   (check (equal? (equal? equal? equal?) (myequal? equal? equal?)))
   (check (equal? (equal? '(1 2 3) '(1 2 3)) (myequal? '(1 2 3) '(1 2 3))))
   (check (equal? (equal? '(123) '(1 2 3)) (myequal? '(123) '(1 2 3))))))

(run-registered-tests)
(run-test '(sicp-2.54-tests))

