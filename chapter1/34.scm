
(define (square x) (* x x))
(define (f g)
  (g 2))

; Let's go through what happens with our first two examples given first.
; (f square)
; (square 2)
; (* 2 2)
; 4
;
; (f (lambda (z) (* z (+ z 1))))
; (* 2 (+ 2 1))
; 6

; (f f) --- Here we're givin a procedure for the paramater of g, as expected.
; (f 2) --- Because we're given f as the procedure, we end up applying f to 2.
; (2 2) --- Because 2 is not a procedure that can be applied to 2, we are given
; Error. -- an error.
