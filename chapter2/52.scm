#lang racket
(require (planet soegaard/sicp:2:1/sicp))

;;; A
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
(define p27 (make-vect .4 .85))
(define p28 (make-vect .6 .85))

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
         (make-segment p25 p26)
         (make-segment p27 p28))))
(paint wave)
;;; =======================================
;;; B
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left up)
              (bottom-right right)
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(paint (corner-split wave 2))
;;; =======================================
;;; C
(define (square-limit painter n)
  (let ((quarter (flip-vert (flip-horiz(corner-split painter n)))))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(paint (square-limit wave 2))