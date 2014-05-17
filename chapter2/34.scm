(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (if (equal? this-coeff (car coefficient-sequence))
                    (+ this-coeff higher-terms)
                    (* x (+ this-coeff higher-terms))))
              0
              coefficient-sequence))