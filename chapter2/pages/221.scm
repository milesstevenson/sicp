(cons 1
      (cons 2
            (cons 3
                  (cons 4 nil))))

(define (one-through-four (list 1 2 3 4)))

(car one-through-four)
(cdr one-through-four)
(cons 10 one-through-four)
(cons 5 one-through-four)

;;; cdr down the whole list
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))
(list-ref squares 3)

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (length items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

;;; cons up an answer list while cdring down a list
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (scale-list items factor)
  (if (null? items)
      '()
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (scale-list items facor)
  (map (lambda (x) (* factor x))
       items))