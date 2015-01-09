#lang racket
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))
;'(a b c)
;
; returns 3
     


;(define x '(a))
;(list x x)
;
; returns 4



; 7 is a bit tricky


; never return at all
;
; (make-cycle (list 3 4 5))