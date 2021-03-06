#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;     ____________________________________________________________________________________
;    |                                  global env                                        |
;    |   factorial: ...                                                                   |
;    |                                                                                    |
;    |____________________________________________________________________________________|

;        ^              ^               ^             ^               ^              ^   
;       _|___          _|___          __|__          _|___          __|__           _|___
;  E1->|     |   E2-> |     |   E3-> |     |   E4-> |     |   E5-> |     |    E6-> |     |
;      |  n:6|        | n:5 |        |  n:4|        |  n:3|        |  n:2|         |  n:1|
;       -----          -----          -----          -----          -----           -----
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;     ____________________________________________________________________________________
;    |                                  global env                                        |
;    |   factorial: ...                                                                   |
;    |   fact-iter: ...                                                                   |
;    |____________________________________________________________________________________|

;       ^    ^         ^                   ^                   ^                    ^
;      _|__  |        _|__________        _|__________        _|__________         _|__________
;     |    | |       |product:   1|      |product:   1|      |product:   2|       |product:   6|
; E1->| n:6| |   E2->|counter:   1|  E3->|counter:   2|  E4->|counter:   3|   E5->|counter:   4| 
;      ----  |       |max-count: 6|      |max-count: 6|      |max-count: 6|       |max-count: 6|
;            |        -------------       -------------       -------------        -------------
;            |
;           ----------------------------------------------
;     ______|_______       _________|_____        _______|______
;     |product:   24|      |product:   120|      |product:   720|  
; E6->|counter:   5 |  E7->|counter:   6  |  E8->|counter:   7  |   
;     |max-count: 6 |      |max-count: 6  |      |max-count: 6  |  
;      -------------        --------------        ---------------   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;