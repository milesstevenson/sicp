#lang racket

;;; It is a necessity to  call on the proc once it's added to action-procedures.
;;; In a scenario that one does not call on proc after initial adding, the signal
;;; of a wire could be effected or not effected at all in an unintentional way.
