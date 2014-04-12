(load "2.scm")

;;;  I spent an unecessary amount of time focusing on
;;; how this data structure could be implemented,
;;; rather than what the specification was asking for.

;;;  By understanding the latter first, you can get a
;;; better idea of what parts of data are necessary
;;; so as not to overcomplicate your implementation.

;;;  For example, my initial thoughts when coming into
;;; this problem were how to go about creating a rect
;;; structure by using the segment data struct created
;;; in the last exercise. This is one method, but doing
;;; so would overcomplicate rect beyond comfort
;;; for finding perimiter and are.
;;; Example rect with this method:
;;; ((((1 4) 3 4) 1 2) 3 2) 

;;;  Because we only need area and perimeter procedures,
;;; meaning only lenght and width are necessary, we could
;;; define our constructor procedure to take two points 
;;; representing two corners to give us the bare  minimum
;;; of a rectangle's length and width. Or as a second way,
;;; we could take in two segments to serve directly as length
;;; and width.

;;;  In other words, I'm a dumbass. Observe what's required
;;; of your spec first and create a process by using
;;; "wishful thinking", THEN consider your constructor
;;; and selectors after. It saves time. This is my view of the
;;; lesson from this exercise, and I think a valuable one. 

;; Abstraction Barriers:
;
;
;; perimeter, area
;; ==================
;; make-rect, width, length
;; ==================
;; cons, cdr, car


;;; Constructor to create a rectangle method1
(define (make-rect-1 point-a point-b)
  ;; Diagnal line through the rectangle.
  (make-segment point-a point-b))

;;; Selectors for make-rect1
(define (length-1 rect)
  (abs (- (x-point (start-segment rect)) 
	  (x-point (end-segment rect)))))
(define (width-1 rect)
  (abs (- (y-point (start-segment rect))
	  (y-point (end-segment rect)))))
;;; ==========================================

;;; Constructor to create a rectangle method1
(define (make-rect-2 p l w)
  ;; Assume these segments are perpendicular
  (cons p (cons l w))  
(define (length-2 rect)
  (cdr (car rect)))
(define (width-2 rect)
  (cdr (cdr rect)))
;;; ==========================================

(define (perimeter rect)
  (* 2 (+ (length-1 rect) (width-1 rect))))
(define (area rect)
  (* (length-1 rect) (width-1 rect)))