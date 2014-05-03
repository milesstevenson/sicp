(load "pages/214.scm")
(load "13.scm")

; Lem is right.

(define A (make-center-percent 3.5 .05)) ;(3.675 . 3.325)
(define B (make-center-percent 4.2 .02)) ;(4.284 . 4.1160000000000005)



(define c (par1 A B)) ;(1.719525065963061 . 2.1158043273753524)

(define d (par2 A B)) ;(1.8392285983066794 . 1.9781002638522427)

(define e (par1 A A)) ;(1.5041666666666669 . 2.0309210526315784)
(define f (par2 A A)) ;(1.6625 . 1.8375)