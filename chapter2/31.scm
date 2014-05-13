(define (tree-map proc items)
  (cond ((null? items) '())
        ((not (pair? items)) (proc items))
        (else (cons (tree-map proc (car items))
                    (tree-map proc (cdr items))))))

(define (square-tree tree) (tree-map square tree))