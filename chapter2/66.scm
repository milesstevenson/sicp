#lang racket

;;; O(logn)
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((= given-key (key (entry set-of-values))) 
         (entry set-of-values))
        ((< given-key (key (entry set-of-values)))
         (lookup given-key (left-branch set-of-records)))
        ((> given-key (key (entry set-of-values)))
         (lookup given-key (right-branch set-of-records)))))