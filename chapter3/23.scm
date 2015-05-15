#lang racket
(require r5rs/init)

(define (make-deque) (cons '() '()))

(define (item node) (car node))
(define (previous node) (cadr node))
(define (next node) (cddr node))
(define (set-previous! node new-node) (set-car! (cdr node) new-node))
(define (set-next! node new-node) (set-cdr! (cdr node) new-node))


(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr! deque item) (set-cdr! deque item))

(define (make-node item prev next) (cons item (cons prev next)))


(define (empty-deque? deque)
  (null? (front-ptr deque)))
  
(define (front-deque deque)
  (cond ((empty-deque? deque)
         (error "FRONT called while deque empty!" deque))
        (else
         (item (front-ptr deque)))))
(define (rear-deque deque)
  (cond ((empty-deque? deque)
         (error "REAR called while deque empty!" deque))
        (else
         (item (rear-ptr deque)))))

(define (front-insert-deque! deque item)
  (let* ((front (front-ptr deque))
         (new-node (make-node item '() front)))
    (cond ((empty-deque? deque)
            (set-front-ptr! deque new-node)
            (set-rear-ptr! deque new-node))
           (else
            (set-previous! front new-node)
            (set-front-ptr! deque new-node))))
  deque)

(define (rear-insert-deque! deque item)
  (let* ((rear (rear-ptr deque))
         (new-node (make-node item rear '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-node)
           (set-rear-ptr! deque new-node))
          (else
           (set-next! rear new-node)
           (set-rear-ptr! deque new-node))))
  deque)

(define (front-delete-deque deque)
  (cond ((empty-deque? deque)
         (error "FRONT-DELETE cannot delete from an empty deque" deque))
        ((null? (next (front-ptr deque)))
         (set-front-ptr! deque '())
         (set-rear-ptr! deque '()))
        (else
         (set-front-ptr! deque (next (front-ptr deque)))
         (set-previous! (front-ptr deque) '())))
  deque)
 
(define (rear-delete-deque deque)
  (cond ((empty-deque? deque)
         (error "REAR-DELETE cannot delete from an empty deque" deque))
        ((null? (previous (rear-ptr deque)))
         (set-front-ptr! deque '())
         (set-rear-ptr! deque '()))
        (else
         (set-rear-ptr! deque (previous (rear-ptr deque)))
         (set-next! (rear-ptr deque) '())))
  deque)

(define (print-deque deque)
  (if (null? (next 
         