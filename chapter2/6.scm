;;; Exercise 2.6

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;;; My interpretation of how (add-1 zero) is evaluated with
;;; the applicative-order substitution model.

(add-1 (lambda (f) (lambda (x) x))) ;evaluate parameters before operators

((lambda (f) (lambda (x) (f ((n f) x))))
 (lambda (f) (lambda (x) x))) ;n is the only place we can subsitute zero

(lambda (f) (lambda (x) (f ((lambda (f) (lambda (x) x)) f) x)))
;;; After this point I'm completely stuck as to what comes next


;; f is applied to (lambda (f) (lambda (x) x)) 
(lambda (f) (lambda (x) (f (((lambda (x) x) x)))))

;; x is applied to (lambda (x) x) -- this is also our value for one
(lambda (f) (lambda (x) (f x)))


;;; one
(define one (lambda (f) (lambda (x) (f x))))

;;; applying f twice gives us two
(define two (lambda (f) (lambda (x) (f (f x)))))


;;; and here is addition
(define (church-add m n) ((lambda (f) (lambda (x) ((m f) ((n f) x))))))


