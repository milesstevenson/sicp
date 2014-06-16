#lang racket
;;; n=5
;;;                    
;;;                    *
;;;                   /  \
;;;                 16     * 
;;;                       /  \
;;;                      8     * 
;;;                           /  \
;;;                          4    * 
;;;                               /  \
;;;                              2     1

;;; n=10 is very similar

;;; In such a tree (for general n), 1 bit would be required to represent the
;;; most frequent symbol. (n-1) bits would be required to represent the least frequent.