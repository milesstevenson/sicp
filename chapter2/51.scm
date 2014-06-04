#lang racket
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))
 (define (below1 painter1 painter2)
   (let ((split-point (make-vect 0.0 0.5)))
     (let ((paint-below
            ((transform-painter (make-vect 0.0 0.0)
                                (make-vect 1.0 0.0)
                                split-point)
             painter1))
           (paint-above
            ((transform-painter split-point
                                (make-vect 1.0 0.5)
                                (make-vect 0.0 1.0))
             painter2)))
       (lambda (frame)
         (paint-below frame)
         (paint-above frame)))))

(define (below2 painter1 painter2)
  (rotate270 (beside (rotate90 painter1) (rotate90 painter2))))