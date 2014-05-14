(define a '(1 2 3))

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x)
                            (cons (car s) x))
                          rest)))))

#|Output

(subsets a)
Value of s: (1 2 3)
Value of s: (2 3)
Value of s: (3)
Value of s: ()
Value of rest inside let: (())


Value of rest inside let: #!unspecific


Value of rest inside let: #!unspecific


;Unspecified return value
|#