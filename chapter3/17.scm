#lang racket
;;; Devise a correct version of the count-pairs procedure of exercise 3.16 that
;;; returns the number of distinct pairs in any structure. 
;;;
;;; Hint: Traverse the structure, maintaining an auxiliary data structure that is
;;; used to keep track of which pairs have already been counted.

;;; This problem discouraged me for a long time, causing me to take a break from the
;;; book for the past 4 months. Admittedly, I also wanted to spend some time studying
;;; algorithms in some detail, but was mostly unsuccessful. Thanks to the many online
;;; resources available, I will continue the purple book journey.

(define (count-pairs x)
  (let ((aux '()))
    (define (helper x)
      (if (or (not (pair? x)) (memq x aux))
          0
          (begin
            (set! aux (cons x aux))
            (+ (helper (car x))
               (helper (cdr x))
               1))))
    (helper x)))
