;;; Old count-leaves from section 2.2.2
(define (count-leaves-old tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) 1)
        (else
         (+ (count-leaves-old (car tree))
            (count-leaves-old (cdr tree))))))
;;; -------------------------------------

(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else
         (append (enumerate-tree (car tree))
                 (enumerate-tree (cdr tree))))))

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) 1)
                       (enumerate-tree t))))
  