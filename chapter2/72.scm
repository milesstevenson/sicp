#lang racket
;;; Most frequent:
;;; (element-of-set? symbol (symbols left-tree)) + (leaf? left-tree)
;;; O(C) Access to the most frequently used symbol would take constant time,
;;; because it will always be the left-most branch containing only one leaf.

;;; Least frequient:
;;; If we're to describe them as in exercise 2.71, there will be (n-1) calls to
;;; element-of-set, which runs in O(n). 
;;; n(n-1) = ~n^2
;;; O(n^2) run time.