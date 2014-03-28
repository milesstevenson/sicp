#|
Exercise 1.4
Observe that our model for evaluation allows for combinations whose
operators are compound expressions. Use this observation to
describe the behavior of the following procedure:
|#

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

#|
Using the substitution model first introduced in pp. 14-15,
evaluation of the combination (a-plus-abs-b 2 (- 1)) would first
retrieve the body of a-plus-abs-b:

((if (> b 0) + -) a b))

The formal parameters would then be replaced by the arguments:

((if (> (- 1) 0) + -) 2 (- 1)))

The problem would then reduce to the evaluation of a combination
with two operands and an operator which is a case analysis. Evaluating 
the case analysis yields the - operator to be used on the operands:

(- 2 (- 1))

Which finally gives the answer 3.
|#
