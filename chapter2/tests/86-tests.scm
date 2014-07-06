#lang racket
(require rackunit rackunit/text-ui)
(require "../86.scm")

(define sicp-2.86-tests
  (test-suite
    "Tests for SICP exercise 2.86"
    
    (test-suite
     "tests for integers"
     (check-equal? (contents (cosine (make-integer 4))) (cos 4))
     (check-equal? (contents (sine (make-integer 4))) (sin 4))
     (check-equal? (contents (arctan (make-integer 4) (make-integer 5))) (atan 4 5))
     (check-equal? (contents (square (make-integer 4))) (* 4 4))
     (check-equal? (contents (square-root (make-integer 4))) (sqrt 4)))
    
    (test-suite
     "test for rationals"
     (check-equal? (contents (cosine (make-rational 4 2))) (cos 2))
     (check-equal? (contents (sine (make-rational 4 2))) (sin 2))
     (check-equal? (contents (arctan (make-rational 4 2) 
                                     (make-rational 8 2))) (atan 2 4))
     (check-equal? (square (make-rational 10 2)) (raise (make-integer 25)))
     (check-equal? (square-root (make-rational 81 9)) (make-real 3.0)))
    
     (test-suite
     "test for reals"
     (check-equal? (contents (cosine (make-real 4))) (cos 4))
     (check-equal? (contents (sine (make-real 4))) (sin 4))
     (check-equal? (contents (arctan (make-real 2) 
                                     (make-real 4))) (atan 2 4))
     (check-equal? (square (make-real 10)) (raise (raise (make-integer 100))))
     (check-equal? (square-root (make-real 81)) (make-real 9.0)))
     
     (test-suite "complex numbers with various coercions and simplifications"
      (check-equal? (add (make-complex-from-real-imag (make-real 1.0) (make-real 2.0))
                         (make-complex-from-real-imag (make-real 3.0) (make-real 4.0)))
                    (make-complex-from-real-imag (make-real 4) (make-real 6)))
      (check-equal? (sub (make-complex-from-real-imag (make-real 3.0) (make-real 5.0))
                         (make-complex-from-real-imag (make-real 1.0) (make-real 2.0)))
                    (make-complex-from-real-imag (make-real 2) (make-real 3)))
      (check-equal? (mul (make-complex-from-mag-ang (make-real 3.0) (make-real 4.0))
                         (make-complex-from-mag-ang (make-real 6.0) (make-real 8.0)))
                    '(complex polar (real . 18.0) real . 12.0))
      (check-equal? (div (make-complex-from-mag-ang (make-real 6.0) (make-real 8.0))
                         (make-complex-from-mag-ang (make-real 3.0) (make-real 4.0)))
                    '(complex polar (real . 2.0) real . 4.0))

      (check-equal? (mul (make-complex-from-mag-ang (make-integer 2) (make-rational 2 1))
                         (make-complex-from-mag-ang (make-real 3) (make-real 4)))
                    '(complex polar (integer . 6.0) integer . 6.0))

      (check-equal? (contents (real-part (make-complex-from-mag-ang (make-real 5)
                                                          (make-real (atan 4 3)))))
                    (* 5 (cos (atan 4 3))))
      (check-equal? (contents (imag-part (make-complex-from-mag-ang (make-real 5)
                                                          (make-real (atan 4 3)))))
                    (* 5 (sin (atan 4 3))))
      
      ;; This is acceptable IMO
      (check-equal? (mul (make-complex-from-mag-ang (make-integer 2) (make-rational 2 1))
                         (make-complex-from-mag-ang (make-real 3) (make-real 4)))
                    (make-complex-from-mag-ang (make-integer 6.0) (make-integer 6.0))))))


(run-tests sicp-2.86-tests)