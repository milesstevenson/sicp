#lang racket
(require r5rs/init)
(require "utils.scm")

;;; 3.30
(define (ripple-carry-adder A B S C)
  (let ((c-in (make-wire)))
    (if (null? (cdr A))
        (set-signal! c-in 0)
        (ripple-carry-adder (cdr A) (cdr B) (cdr S) c-in))
    (full-adder (car A) (car B) c-in (car S) C))
  'done)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Test ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   1 0 1
;;; + 0 0 1
;;;   -----
;;;   1 1 0
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   ^ C = 0
;;;   |
;;;   |  A_1 = 1, B_1 = 0
;;;   |   |        |
;;;   -[ FA ]  <---
;;;       |  ^
;;;       |  |              C = 0
;;;   S = 1  --------------------
;;;                              |  A_2 = 0, B_2 = 0
;;;                              |     |        |
;;;                              |     |        |
;;;                              ----> [ FA ]  <-
;;;                                      |  ^
;;;                                      |  |                     C = 1
;;;                                  S = 1  ----------------------------
;;;                                                                     | A_3 = 1, B_3 = 1
;;;                                                                     |     |         |
;;;                                                                     |     |         |
;;;                                                                     ---- [ FA ]^   <-
;;;                                                                             |  |               C = 0
;;;                                                                         S = 0  -------------------- 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A_k
(define x1 (make-wire))
(define x2 (make-wire))
(define x3 (make-wire))
(define a-list (list x1 x2 x3))

(set-signal! x1 1)
(set-signal! x2 0)
(set-signal! x3 1)

; B_k
(define y1 (make-wire))
(define y2 (make-wire))
(define y3 (make-wire))
(define b-list (list y1 y2 y3))

(set-signal! y1 0)
(set-signal! y2 0)
(set-signal! y3 1)

; S_k
(define z1 (make-wire))
(define z2 (make-wire))
(define z3 (make-wire))
(define s-list (list z1 z2 z3))

; C
(define c (make-wire))

