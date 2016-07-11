#lang racket

;; We must preserve the queue structure because these operations are being added
;; to the data structure as they are traversed in the circuit, so in left-to-right
;; fashion. If these operations were traversed in first-in-first out, right-to-left,
;; fashion, the conclusion would be garbage.
