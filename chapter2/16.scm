;;; Dealing with equivalent algebraic expressions may lead to different
;;; answers if the objects of those algebraic expressions are intervals,
;;; or similar to intervals in that they don't algebraic certainty.

;;; An interval-arithmetic package that does this shortcoming cannot be
;;; created. According to wikipedia, the general rule is that the more
;;; any same interval is used in a function, the more uncertainty can be
;;; expected from the value. I assume this general rule can be applied
;;; to objects similar, in algebraic consistency, to intervals as well.