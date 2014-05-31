#lang racket/gui

;;; PictureLanguage.scm
;;; 
;;; This file provides the basic implementation of the Picture Language.
;;; It includes definitions for both the higher-level procedures used
;;; for combinning painters via different patterns and lower-level
;;; procedures for creating primitive painters, frames, segements and vectors.
;;;
;;; To render the constructed images, the implementation 
;;; uses standard PLT Racket libraries for creating bitmaps, frames and
;;; canvases. For more information consult the PLT documentation:
;;; http://docs.racket-lang.org/gui/drawing-overview.html
;;;
;;; The code written is an adaptation of code from the book:
;;; "Structure and Interpretation of Computer Programs", Section 2.2.4 
;;; The book can be found at: http://http://mitpress.mit.edu/sicp/full-text/book/book.html
;;; 
;;; Author: Ivan Vladimirov Ivanov, 2010 
;;; Email: ivan.vladimirov.ivanov@gmail.com
;;; 
;;; Version: 1.0

;; Some terminology:
;;
;; "Painter" is a class of procedures that take a signle argument of type
;; frame and draw some picture in the space specified by the frame.
;;
;; A "Frame" is a data-structure consisting of 3 vectors: origin, edge1 and
;; edge2. The origin specifies the offset of the frame's origin from some 
;; absolute origin in the plane, and the edge vectors specify the offsets of
;; the frame's corners from its origin.

;; -------------------------------------------------------------------------
;; Procedures for transforming and combinning painters
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (flip-horiz painter)
  (transform-painter 
   painter
   (make-vect 1.0 0.0)
   (make-vect 0.0 0.0)
   (make-vect 1.0 1.0)))

(define (identity painter) painter)

(define (rotate90 painter)
  (transform-painter painter 
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left 
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(define (below painter1 painter2)
  (rotate90 (beside (rotate270 painter1)
                    (rotate270 painter2))))

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))

(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert identity flip-vert)))
    (combine4 painter)))

;; Picture looks inward.
(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

; Picture looks outward.
;(define (square-limit painter n)
;  (let ((combine4 (square-of-four flip-horiz identity rotate180 flip-vert)))
;    (combine4 (corner-split (flip-horiz painter) n))))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter 
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

;; -------------------------------------------------------------------------
;; Procedure for constructing painters out of line segments.
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each 
     (lambda (segment)
       (draw-line 
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

;; -------------------------------------------------------------------------
;; Implementation of the frame abstraction.
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (caddr frame))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect 
     (origin-frame frame)
     (add-vect 
      (scale-vect (xcor-vect v)
                  (edge1-frame frame))
      (scale-vect (ycor-vect v)
                  (edge2-frame frame))))))

;; -------------------------------------------------------------------------
;; Implementation of the segment abstraction.
(define (make-segment start end)
  (cons start end))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

;; -------------------------------------------------------------------------
;; Implementation of the vector abstraction.
(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1)
                (xcor-vect v2))
             (+ (ycor-vect v1)
                (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1)
                (xcor-vect v2))
             (- (ycor-vect v1)
                (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

;; -------------------------------------------------------------------------
;; Setting up the drawing infrastructure specific to PLT Racket.
(define size-x 250)
(define size-y 250)

(define picture (make-object bitmap% size-x size-y))
(define bm-dc (make-object bitmap-dc% picture))
(send bm-dc clear)

(define frame-gui (new frame% 
                       [label "Picture Language"]
                       [width (+ size-x 10)]
                       [height (+ size-y 35)]))

(define canvas (new canvas%
                    [parent frame-gui]
                    [paint-callback
                     (lambda (canvas dc)
                       (send dc draw-bitmap picture 0 0))]))

(define (draw-line v1 v2)
  (send bm-dc 
        draw-line 
        (* size-x (xcor-vect v1)) 
        (- size-y (* size-y (ycor-vect v1)))
        (* size-x (xcor-vect v2))
        (- size-y (* size-y (ycor-vect v2)))))

(send frame-gui show #t)


;; -------------------------------------------------------------------------
;; Definitions for the various segments->painters. 

(define frame (make-frame (make-vect 0 0)
                          (make-vect 1 0)
                          (make-vect 0 1)))

(define outline
  (segments->painter 
   (list (make-segment (make-vect 0.0 0.0) (make-vect 1.0 0.0))
         (make-segment (make-vect 1.0 0.0) (make-vect 1.0 1.0))
         (make-segment (make-vect 1.0 1.0) (make-vect 0.0 1.0))
         (make-segment (make-vect 0.0 1.0) (make-vect 0.0 0.0)))))

(define cross
  (segments->painter
   (list (make-segment (make-vect 0.0 0.0) (make-vect 1.0 1.0))
         (make-segment (make-vect 1.0 0.0) (make-vect 0.0 1.0)))))

(define diamond 
  (segments->painter
   (list (make-segment (make-vect 0.0 0.5) (make-vect 0.5 0.0))
         (make-segment (make-vect 0.5 0.0) (make-vect 1.0 0.5))
         (make-segment (make-vect 1.0 0.5) (make-vect 0.5 1.0))
         (make-segment (make-vect 0.5 1.0) (make-vect 0.0 0.5)))))

(define wave 
  (segments->painter 
   (list (make-segment (make-vect 0.25 0.00) (make-vect 0.37 0.37)) ;1
         (make-segment (make-vect 0.40 0.00) (make-vect 0.50 0.25)) ;2
         (make-segment (make-vect 0.50 0.25) (make-vect 0.62 0.00)) ;3
         (make-segment (make-vect 0.75 0.00) (make-vect 0.70 0.50)) ;4
         (make-segment (make-vect 0.70 0.50) (make-vect 1.00 0.30)) ;5
         (make-segment (make-vect 1.00 0.50) (make-vect 0.75 0.62)) ;6
         (make-segment (make-vect 0.75 0.62) (make-vect 0.62 0.62)) ;7
         (make-segment (make-vect 0.62 0.62) (make-vect 0.75 0.75)) ;8
         (make-segment (make-vect 0.75 0.75) (make-vect 0.62 1.00)) ;9
         (make-segment (make-vect 0.40 1.00) (make-vect 0.30 0.75)) ;10
         (make-segment (make-vect 0.30 0.75) (make-vect 0.40 0.62)) ;11
         (make-segment (make-vect 0.40 0.62) (make-vect 0.25 0.62)) ;12
         (make-segment (make-vect 0.25 0.62) (make-vect 0.20 0.50)) ;13
         (make-segment (make-vect 0.20 0.50) (make-vect 0.00 0.70)) ;14
         (make-segment (make-vect 0.37 0.37) (make-vect 0.30 0.50)) ;15
         (make-segment (make-vect 0.30 0.50) (make-vect 0.12 0.37)) ;16
         (make-segment (make-vect 0.12 0.37) (make-vect 0.00 0.50)) ;17
         (make-segment (make-vect 0.50 0.70) (make-vect 0.35 0.75)) ;smile 1
         (make-segment (make-vect 0.50 0.70) (make-vect 0.65 0.75)) ;smile 2
         )))

;; -------------------------------------------------------------------------
;; Testing
(define wave2 (beside wave (flip-vert wave)))
(define wave4 (flipped-pairs wave))

;((flip-vert wave) frame)
;((flip-horiz wave) frame)
;((rotate180 wave) frame)
;((squash-inwards wave4) frame)
;(wave4 frame)
;((right-split wave4 6) frame)
;((up-split wave4 6) frame)
;((squash-inwards (square-limit wave 6)) frame)
;((square-limit wave 6) frame)
;((square-limit (square-limit wave 2) 2) frame)
;((squash-inwards (square-limit (square-limit wave 2) 2)) frame)

;(wave2 frame)
;((right-split outline 6) frame)
;((squash-inwards (square-limit cross 4)) frame)
((square-limit diamond 4) frame)


