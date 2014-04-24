(load "7.scm")
(load "pages/214.scm")

(define (width intrvl)
  ;; The width of an interval is half of the difference between
  ;; its upper and lower bounds.
  (/ (- (upper-bound intrvl) (lower-bound intrvl)) 2))

;;; Show that the width of the sum of two intervals is a function
;;; only on the widths of the intervals being added (or subtracted).
;;; Give examples to show that this is not true for multiplication
;;; or division.



(define a (make-interval 10 5))
(define b (make-interval 15 10))
(define c (add-interval a b))
(define g (sub-interval a b))

;;; This shows that (width (add-interval a b)) is equal to
;;; (+ (width a) (width b))
;;; The same holds for sub-interval.
(width c) ; 5
(+ (width a) (width b)) ; 5

(width g) ; 0
(- (width a) (width b)) ; 0

;;; Let's see if it's holds for mul-interval and div-interval...

(define d (mul-interval a b))
(define e (div-interval a b))


;;; Nope.
(width d) ; 50
(* (width a) (width b)) ; 25/4

(width e) ; .33333333333333337
(/ (width a) (width b)) ; 1


;;; ============================================================

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))





