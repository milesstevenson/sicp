;;; reverse-deep takes a list as argument and returns
;;; as its value the list with its elements reversed
;;; and with all the sublists deep-reversed as well.
(define (reverse-deep items)
  (define (reverse-deep-iter mark)
    (cond ((equal? mark -1) '())
          ((pair? (list-ref items mark))
                  (cons
                   (reverse-deep (list-ref items mark))
                   (reverse-deep-iter (- mark 1))))
          (else
           (cons (list-ref items mark)
                 (reverse-deep-iter (- mark 1))))))
  (reverse-deep-iter (- (length items) 1)))