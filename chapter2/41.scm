(define (distinct-sum n s)
  (define (triple-sum? triple)
    (equal? s (accumulate + 0 triple)))
  (filter triple-sum? (flatmap
                       (lambda (i)
                         (flatmap (lambda (j) (map (lambda (k) (list i j k))
                                                   (enumerate-interval 1 (- j 1))))
                                  (enumerate-interval 1 (- i 1))))
                       (enumerate-interval 1 n))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))



