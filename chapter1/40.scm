;; Exercise 1.40

; Utilities
(define (average a b)
  (/ ( + a b) 2))
(define (average-damp f)
  (lambda (x) (average x (f x))))
(define (cube x) (* x x x))
(define (square x) (* x x))

; Find fixed-point
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try  next))))
  (try first-guess))

; Find derivatives
(define dx 0.00001)
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x)) 
       dx)))
; f(x) = x - g(x)/Dg(x)
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

; Apply newton's method
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

;--------------------------
;1.40 solution
(define (cubic a b c)
 (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

(newtons-method (cubic 2 3 4) 1)


