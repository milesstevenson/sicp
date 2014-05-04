(define (last-pair items)
  (if (null? items)
      (error "You've provided improper input -- empty list"))
  (if (null? (cdr items))
      items
      (last-pair (cdr items))))