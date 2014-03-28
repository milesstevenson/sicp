(gcd 206 40)

(if (= 40 0)
    206
    (gcd 40 (remainder 206 40)))
;------------------------------------- finished 1 iteration
(gcd 40 (remainder 206 40))

(if (= (remainder 206 40) 0) ; remainder evaluated once -- 6!
    40
    (gcd (remainder 206 40) (remainder 40 (remainder 206 40))))
;----------------------------------------------------------------- finished 2 iterations
(gcd (remainder 206 40) (remainder 40 (remainder 206 40))))

(if (= (remainder 40 (remainder 206 40)) 0) ; remainder evaluated three times -- 4!
    (remainder 206 40)
    (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
;------------------------------------------------------------------------------------------------------------- finished three iterations
(gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))

(if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0) ; remainder evaluated seven times -- 2!
    (remainder 40 (remainder 206 40))
    (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
    (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
;---------------------------------------------------------------------------------------------------------------------- finished three iterations
(gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
    (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
    
    (if (= (remainder (remainder 40 (remainder 206 40)) 
                      (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 0) ; remainder evaluaed fourteen times -- 0!
        (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) ; remainder evaluated eighteen times -- 2!
        ; ... no longer need the else!
        )
;---------------------------------------------------------------------------------------------------------------------------
;> 2

(gcd 206 40)

(if (= 40 0)
    206
    (gcd 40 (remainder 206 40)))
;-----------------------------------------------
(gcd 40 (remainder 206 40)) ; remainder evaluatd once -- 6

(gcd 40 6)

(if (= 6 0)
    40
    (gcd 6 (remainder 40 6)))
;------------------------------------------------
(gcd 6 (remainder 40 6)) ; remainder evaluated twice -- 4

(gcd 6 4)

(if (= 6 0)
    6
    (gcd 4 (remainder 6 4)))
;--------------------------------------------------
(gcd 4 (remainder 6 4)) ; remainder evaluated three times -- 2

(gcd 4 2)

(if (= 2 0)
    4
    (gcd 2 (remainder 4 2)))
;---------------------------------------------------
(gcd 2 (remainder 4 2)) ; remainder evaluated four times -- 0

(gcd 2 0)

(if (= 0 0)
    2
    (gcd 2 (remainder 2 0)))
;------------------------------------------------------
;>2
   
