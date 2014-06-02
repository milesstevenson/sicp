#lang racket
(require (planet soegaard/sicp:2:1/sicp))
;;; Be certain to run this in the Racket IDE


(define p1 (make-vect 0.0 0.0))
(define p2 (make-vect 0.0 .99))
(define p3 (make-vect .99 0.0))
(define p4 (make-vect .99 .99))

;;;a
(define outline
  (segments->painter
   (list (make-segment p1 p3)
         (make-segment p1 p2)
         (make-segment p3 p4)
         (make-segment p2 p4))))
(paint outline)

;;;b
(define cross
  (segments->painter
   (list (make-segment p1 p4)
         (make-segment p3 p2))))
(paint cross)

;;;c
(define diamond
  (segments->painter
   (list (make-segment (make-vect 0.5 0.99) (make-vect 0.0 0.5))
         (make-segment (make-vect 0.5 0.99) (make-vect 0.99 0.5))
         (make-segment (make-vect 0.0 0.5) (make-vect 0.5 0.0))
         (make-segment (make-vect 0.5 0.0) (make-vect 0.99 0.5)))))
(paint diamond)

;;; d
(define p5 (make-vect .25 0))
(define p6 (make-vect .35 .5))
(define p7 (make-vect .3 .6))
(define p8 (make-vect .15 .4))
(define p9 (make-vect 0 .65))
(define p10 (make-vect .4 0))
(define p11 (make-vect .5 .3))
(define p12 (make-vect .6 0))
(define p13 (make-vect .75 0))
(define p14 (make-vect .6 .45))
(define p15 (make-vect 1 .15))
(define p16 (make-vect 1 .35))
(define p17 (make-vect .75 .65))
(define p18 (make-vect .6 .65))
(define p19 (make-vect .65 .85))
(define p20 (make-vect .6 1))
(define p21 (make-vect .4 1))
(define p22 (make-vect .35 .85))
(define p23 (make-vect .4 .65))
(define p24 (make-vect .3 .65))
(define p25 (make-vect .15 .6))
(define p26 (make-vect 0 .85))

(define wave
  (segments->painter
   (list (make-segment p5 p6)
         (make-segment p6 p7)
         (make-segment p7 p8)
         (make-segment p8 p9)
         (make-segment p10 p11)
         (make-segment p11 p12)
         (make-segment p13 p14) 
         (make-segment p14 p15)
         (make-segment p16 p17) 
         (make-segment p17 p18)
         (make-segment p18 p19)
         (make-segment p20 p21)
         (make-segment p21 p22) 
         (make-segment p22 p23)
         (make-segment p23 p24)
         (make-segment p24 p25)
         (make-segment p25 p26))))
(paint wave)