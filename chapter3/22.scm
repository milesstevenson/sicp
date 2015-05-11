#lang racket
(require r5rs/init)
  

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty-queue?)
      (null? front-ptr))
    (define (front)
      (if (empty-queue?)
          (error "FRONT called with an empty queue" front-ptr)
          (car front-ptr)))
    (define (insert! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair))
              (else
               (set-cdr! rear-ptr new-pair)
               (set! rear-ptr new-pair)))))
    (define (delete!)
      (cond ((empty-queue?)
             (error "DELETE! called with an empty queue" front-ptr))
            (else
             (set! front-ptr (cdr front-ptr)))))
    (define (dispatch m)
            (cond ((eq? m 'insert-queue!) insert!)
                  ((eq? m 'front-queue) front)
                  ((eq? m 'delete-queue!) delete!)
                  (else 
                   (error "Unknown request -- MAKE-QUEUE" m))))
    dispatch))
             
(define (insert-queue! q item)
  ((q 'insert-queue!) item))

(define (front-queue q)
  ((q 'front-queue)))

(define (delete-queue! q)
  ((q 'delete-queue!)))