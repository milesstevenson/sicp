#lang racket
;;; Changes necessary to add to a system in order to add new types or new operations:
;;;
;;; 1. generic operations with explicit dispatch
;;; In order to add a new type, the current procedures that use dispatch on type
;;; must add the new data-type to be checked for in their conditional check.
;;;
;;; In order to add a new operation, the operation needs to do a dispatch of the type
;;; of data it has been handed, to choose the correct procedure to handle the type given.
;;;
;;; 2. data-directed programming
;;; In order to add a new type, ensure that your package interface procedures point your
;;; operations to a new column reserved just for your data-type within a data-directed table.
;;;
;;; In order to add a new operation, ensure that your package interface points your
;;; operation to a new row, tagged with the same column tag you use for the rest of
;;; your operations on this particular data type, in a data-directed table.
;;;
;;; 3. message-passing
;;; In order to add a new type, create a new constructor for your data type. Have it
;;; use an internal procedure that does dispatch on operation. That means you can pass
;;; the constructor the selector name, and get the appropriate results
;;;
;;; In order to add a new operation, within the dispatch in your constructor, add a case
;;; for the new operation you want to add, and what the its appropriate result should be.

;;; To me it seems like message-passing would be the most ideal strategy for a system
;;; where new operations need to often be added. It also seems the most clean and simple
;;; to deal with.