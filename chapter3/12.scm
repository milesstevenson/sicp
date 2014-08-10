#lang racket
(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define x (list 'a 'b))
(define y (list 'c 'd))

;     w:                        
;     +---+---+    +---+---+    
;   x:| * | *-|----|-* | * |-------    
;     +-|-+---+    +-|-+---+      |
;       |            |            |
;     +-|-+        +-|-+          |
;     |'a |        |'b |          |
;     +---+        +---+          |
;                                 |
;     z:                          |
;     +---+---+    +---+---+    +---+---+    +---+---+
;     | * | *-|----|-* | * |--y:| * | * |----| * | / |
;     +-|-+---+    +-|-+---+    +---+---+    +---+---+
;       |            |            |            |
;     +-|-+        +-|-+        +-|-+        +---+
;     |'a |        |'b |        |'c |        |'d |
;     +---+        +---+        +---+        +---+
;
; > (cdr x)
; > '(b)
;
; > (cdr x)
; > '(b c d)