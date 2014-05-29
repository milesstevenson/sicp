(load "../../test-manager/load.scm")
(load "../37.scm")

(in-test-group
 sicp-2.37-tests

 (define-test (standard-tests)
   (check (equal? (matrix-*-vector '((1 2) (3 4))
                                   '(5 6))
                  '(17 39)))
   (check (equal? (transpose '((1 2 3)
                               (4 5 6)
                               (7 8 9)))
                  '((1 4 7)
                    (2 5 8)
                    (3 6 9))))

   (check (equal? (matrix-*-matrix '((1 2)
                                     (3 4))
                                   '((5 6)
                                     (7 8)))
                  '((19 22)
                    (43 50))))))
(run-registered-tests)
(run-test '(sicp-2.37-tests))
