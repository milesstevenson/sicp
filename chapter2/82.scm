 #lang racket
;;; Testing for this generic procedure taken from skanev: 
;;; https://github.com/skanev/playground/blob/master/scheme/sicp/02/82.scm
;;;
;;; He's done an excellent job at the exercises and I have no shame in saying
;;; when I get stuck I go directly to his solutions for guidance on a proper approach.
;;; ============================================================================

(define table1 (make-hash))
(define (put op type item)
  (hash-set! table1 (list op type) item))
(define (get op type)
  (hash-ref table1 (list op type) false))

(define table2 (make-hash))
(define (put-coercion op type element)
  (dict-set! table2 (list op type) element))
(define (get-coercion op type)
  (dict-ref table2 (list op type) false))
;;; ============================================================================

(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else
         (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else
         (error "Bad tagged datum -- CONTENTS" datum))))


(define (apply-generic op . args)
  ;; Here we're trying to generalize apply-generic to handle any number of types. 
  ;; The trouble here will be that we do not have any previously saved data on the
  ;; types we're dealing with, regarding whether or not one is a sub-type or
  ;; super-type of the other. This would make things much easier and result in a
  ;; faster run-time after all the type relations were loaded once.
  ;;
  ;; Because that's not the case, this procedure will need to convert each data
  ;; type one by one. If all of the types can be converted and there is an
  ;; operation available for said converted types, we have success, else we need
  ;; to start over again with the next type in the list.
  (define (coerce-to target-type remaining-args result)
    (if (null? remaining-args)
        result
        (let* ((arg (car remaining-args))
               (original-type (type-tag arg)))
          (if (eq? target-type original-type)
              (coerce-to target-type 
                         (cdr remaining-args)
                         (append result (list arg)))
              (let ((original->target (get-coercion original-type target-type)))
                (if original->target
                    (coerce-to target-type
                               (cdr remaining-args)
                               (append result (list (original->target arg))))
                    #f))))))
  (define (apply-generic-iter arg-types remaining-args)
    (if (null? arg-types)
        (error "No method for these types" (list op (map type-tag args)))
        (let* ((target-type (car arg-types))
              (coerced-args (coerce-to target-type remaining-args '())))
          (if coerced-args
              (let ((proc (get op (map type-tag coerced-args))))
                (if proc
                    (apply proc (map contents coerced-args))
                    (apply-generic-iter (cdr arg-types) remaining-args)))
              (apply-generic-iter (cdr arg-types) remaining-args)))))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
           (apply proc (map contents args))
           (apply-generic-iter type-tags args)))))
         
          
          


;;; constructors for types =====================================================
(define (make-a) (attach-tag 'a "a"))
(define (make-b) (attach-tag 'b "b"))
(define (make-c) (attach-tag 'c "c"))

;;; coercion operation =========================================================
(put-coercion 'a 'b (lambda (x) (attach-tag 'b (string-append (contents x) "->" "b"))))
;(put-coercion 'a 'c (lambda (x) (attach-tag 'c (string-append (contents x) "->" "c"))))
(put-coercion 'b 'a (lambda (x) (attach-tag 'a (string-append (contents x) "->" "a"))))
(put-coercion 'b 'c (lambda (x) (attach-tag 'c (string-append (contents x) "->" "c"))))
;(put-coercion 'c 'a (lambda (x) (attach-tag 'a (string-append (contents x) "->" "a"))))
;(put-coercion 'c 'b (lambda (x) (attach-tag 'b (string-append (contents x) "->" "b"))))

;;; silly generic operations ===================================================
(define (foo x y) (apply-generic 'foo x y))
(define (bar x y z) (apply-generic 'bar x y z))
(define (baz w x y z) (apply-generic 'baz w x y z))

;;; specific proceduess ===================================================
(put 'foo '(a a) (lambda args (cons 'foo-a-a (map string->symbol args))))
(put 'foo '(b b) (lambda args (cons 'foo-b-b (map string->symbol args))))
(put 'foo '(a a a) (lambda args (cons 'foo-a-a-a (map string->symbol args))))
(put 'foo '(b b b) (lambda args (cons 'foo-b-b-b (map string->symbol args))))
(put 'foo '(a a a a) (lambda args (cons 'foo-a-a-a-a (map string->symbol args))))
(put 'foo '(b b b b) (lambda args (cons 'foo-b-b-b-b (map string->symbol args))))