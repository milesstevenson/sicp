(load "../../test-manager/load.scm")
(load "../3.scm")

(in-test-group
 sicp-2.03-tests

 (define-test (rect-test-1)
   "Checking that make-rect-1 behaves properly!"
   (interaction
    (define a (make-point 4 0))
    (define b (make-point 0 8))
    (define rect1 (make-rect-1 a b))
    (length-1 rect1)
    (produces 4)
    (width-1 rect1)
    (produces 8)
    (perimeter rect1)
    (produces 24)
    (area rect1)
    (produces 32)

(define-test (rect-test-2)
   "Checking that make-rect-1 behaves properly!"
   (interaction
    (define a (make-point 4 0))
    (define b (make-point 0 8))
    (define rect1 (make-rect-2 a b))
    (length-2 rect2)
    (produces 4)
    (width-2 rect2)
    (produces 8)
    (perimeter rect2)
    (produces 24)
    (area rect2)
    (produces 32))))))


(run-registered-tests)

(run-test '(sicp-2.03-tests))
    