#lang racket
;;; I'm not 100% sure that this is the exact process, but I think it's safe to assume
;;; that it is something similar. acc has only been modeled here instead of both acc
;;; and acc2, but because acc2 is a completely new object, a new procedure object
;;; would be created which creates a new frame and environment. The only resources
;;; shared between acc and acc2 would be the global environment!

;     _____________________________________________________________________________________
;    |                                  global env                                         |
;    |   make-account: -----------------------------------------------------------------   |
;    |   acc:+                                                                         |   |
;    |       |                                                                         |   |
;    |_______|_________________________________________________________________________|___|
;            |               ^                                                         |  ^
;            |               |                                                        O O-|
;            |               |                                                       ...
;            |               |
;            |       ________|____
;           O O---->| balance: 50 |
;          ...      | withdraw:   |
;             E1--> | deposit:    |
;                   | depatch:    |
;                    -------------

;     _____________________________________________________________________________________
;    |                                  global env                                         |
;    |   make-account: -----------------------------------------------------------------   |
;    |   acc:+                                                                         |   |
;    |       |                                                                         |   |
;    |_______|_________________________________________________________________________|___|
;            |               ^                                                         |  ^
;            |               |                                                        O O-|
;            |               |                                                       ...
;            |               |
;            |       ________|____
;           O O---->| balance: 50 |
;          ...      | withdraw:   |
;             E1--> | deposit:    |
;                   | depatch:    |
;                    -------------
;                    ^        ^
;                    |        |______________
;               _____|______          ______|____
;              |m: 'deposit |        |amount: 40 |
;         E2-> |____________|   E3-> |___________|

;     _____________________________________________________________________________________
;    |                                  global env                                         |
;    |   make-account: -----------------------------------------------------------------   |
;    |   acc:+                                                                         |   |
;    |       |                                                                         |   |
;    |_______|_________________________________________________________________________|___|
;            |               ^                                                         |  ^
;            |               |                                                        O O-|
;            |               |                                                       ...
;            |               |
;            |       ________|____
;           O O---->| balance: 90 |
;          ...      | withdraw:   |
;             E1--> | deposit:    |
;                   | depatch:    |
;                    -------------

;     _____________________________________________________________________________________
;    |                                  global env                                         |
;    |   make-account: -----------------------------------------------------------------   |
;    |   acc:+                                                                         |   |
;    |       |                                                                         |   |
;    |_______|_________________________________________________________________________|___|
;            |               ^                                                         |  ^
;            |               |                                                        O O-|
;            |               |                                                       ...
;            |               |
;            |       ________|____
;           O O---->| balance: 90 |
;          ...      | withdraw:   |
;             E1--> | deposit:    |
;                   | depatch:    |
;                    -------------
;                    ^        ^
;                    |        |______________
;               _____|______          ______|____
;              |m: 'withdraw|        |amount: 60 |
;         E2-> |____________|   E3-> |___________|

;     _____________________________________________________________________________________
;    |                                  global env                                         |
;    |   make-account: -----------------------------------------------------------------   |
;    |   acc:+                                                                         |   |
;    |       |                                                                         |   |
;    |_______|_________________________________________________________________________|___|
;            |               ^                                                         |  ^
;            |               |                                                        O O-|
;            |               |                                                       ...
;            |               |
;            |       ________|____
;           O O---->| balance: 30 |
;          ...      | withdraw:   |
;             E1--> | deposit:    |
;                   | depatch:    |
;                    -------------