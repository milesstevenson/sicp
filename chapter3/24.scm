#lang racket
(require r5rs/init)

(define (make-table same-key?)
  (define (assoc-2 key records)
    (cond ((null? records) false)
          ((same-key? key (caar records)) (car records))
          (else (assoc-2 key (cdr records)))))
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc-2 key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc-2 key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc-2 key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc-2 key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable 
                            (cons (cons key-2 value)
                                           (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))


;; put a silly lambda in that meets your key checking needs..
(define operation-table (make-table (lambda (key1 key2)
                                      (if (and (number? key1) (number? key2))
                                          (< (- (abs key1) (abs key2)) .01)
                                          (equal? key1 key2)))))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))