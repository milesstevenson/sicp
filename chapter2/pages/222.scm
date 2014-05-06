(define x (cons (list 1 2) (list 3 4)))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((pair? x) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))