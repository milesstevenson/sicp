;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The "eight-queens puzzle" asks how to place eight queens on a chessboard so that no queen
;;; is in check from any other (i.e., no two queens are in the same row, column, or diagonal).
;;; One possible solution is shown in the figure below.
;;;
;;; +--+--+--+--+--+--+--+--+
;;; |  |  |  |  |  |Q |  |  |
;;; +--+--+--+--+--+--+--+--+
;;; |  |  |Q |  |  |  |  |  |
;;; +--+--+--+--+--+--+--+--+
;;; |Q |  |  |  |  |  |  |  |
;;; +--+--+--+--+--+--+--+--+
;;; |  |  |  |  |  |Q |  |  |
;;; +--+--+--+--+--+--+--+--+
;;; |  |  |  |  |Q |  |  |  |
;;; +--+--+--+--+--+--+--+--+
;;; |  |  |  |  |  |  |  |Q |
;;; +--+--+--+--+--+--+--+--+
;;; |  |Q |  |  |  |  |  |  |
;;; +--+--+--+--+--+--+--+--+
;;; |  |  |  |Q |  |  |  |  |
;;; +--+--+--+--+--+--+--+--+
;;;
;;; One way to solve the puzzle is to work across the board, placing a queen in each column.
;;; Once we have placed k-1 queens, we must place the kth queen in a position where it does
;;; not check any of the queens already on the board.
;;;
;;; We can formulate this approach recursively:
;;;
;;; Assume that we have already generated the sequence of all possible ways to place k-1
;;; queens in the first k-1 columns of the board. For each of these ways, generate an extended
;;; set of positions by placing a queen in each row of the kth column. Now filter these, keeping
;;; only the positions for which the queen in the kth column is safe with respect to the other
;;; queens. This produces the sequence of all ways to place k queens in the first k columns.
;;; By continuing this process, we will produce not only one solution, but all solutions to
;;; the puzzle.
;;;
;;; We implement this solution as a procedure queens, which returns a sequence of all solutions
;;; to the problem of placing n queens on an (n x n) chessboard. Queens has an internal procedure
;;; queen-cols that returns the sequence of all ways to place queens in the first k columns of
;;; the board.
;;;
;;; (define (queens board-size)
;;;   (define (queen-cols k)
;;;     (if (= k 0)
;;;         (list empty-board)
;;;         (filter
;;;          (lambda (positions) (safe? k positions))
;;;          (flatmap
;;;           (lambda (rest-of-queens)
;;;             (map (lambda (new-row)
;;;                    (adjoin-position new-row k rest-of-queens))
;;;                  (enumerate-interval 1 board-size)))
;;;           (queen-cols (- k 1))))))
;;;   (queen-cols board-size))
;;;
;;; In this procedure rest-of-queens is a way to place k-1 queens in the first k-1 columns,
;;; and new-row is a proposed row in which to place the queen for the kth column. Complete
;;; the program by implementing the representation for sets of board positions, including
;;; the procedure adjoin-position, which adjoins a new row-column position to a set of
;;; positions, and empty-board, which represents an empty set of positions. You must
;;; also write the procedure safe?, which determines for a set of positions, whether the
;;; queen in the kth column is safe with respect to the others. (Note that we need only
;;; check whether the new queen is safe - the other queens are already guaranteed safe
;;; with respect to each other).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
       (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row
                                    k
                                    rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (if (> board-size 4)
      (length (queen-cols board-size))
      (queen-cols board-size)))
;;;---------------------------------------------------------
;;; board constructor
(define empty-board '())

;;; queen constructors
(define (make-queen row col)
  (cons row col))
(define (adjoin-position row col positions)
  (cons (make-queen row col) positions))

;;; queen selectors
(define (queen-column queen)
  (cdr queen))
(define (queen-row queen)
  (car queen))
;;;---------------------------------------------------------
(define (safe? k positions)
 (and (safe?-row (map car positions))
      (safe?-diag (car positions)
                  (cdr positions))))


(define (contains? item items)
  (cond ((null? items) #f)
        ((= item (car items)) #t)
        (else (contains? item (cdr items)))))
(define (safe?-row positions)
  (not (contains? (car positions)
                 (cdr positions))))

(define (safe?-diag queen positions)
  (cond ((null? positions)
         #t)
      ((= (abs (- (queen-column queen)
                  (queen-column (car positions))))
          (abs (- (queen-row queen)
                  (queen-row (car positions)))))
       #f)
      (else
              (safe?-diag queen (cdr positions)))))
 
;;;---------------------------------------------------------    
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))
(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

;-----------
