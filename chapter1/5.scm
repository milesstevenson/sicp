

#|
Exercise 1.5
Ben Bitdiddle has invented a test to determine whether
the interpreter he is faced with is using applicative-order
or normal-order evaluation. He defines the following procedures:
|#

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

#| Then he evaluates the expression |#

(test 0 (p))

#|
What behavior will Ben observe with an interpreter that uses
applicative-order? Normal-order evaluation? Explain your answer.
(Assume that the evaluation rule for the special form if is the
same whether the interpreter is using normal or applicative order:
The predicate expression is evaluated first, and the result
determines whether to evaluate the consequent or the alternative
expression.)

ANSWER:

In applicative-order, the evaluation of test first examine the
arguments, while in normal-order, the operands would first
be examined. If normal-order were the way the Scheme interpreter 
evaluated procedures, the behavior of this program would not be unusual.
Because Scheme's interpreter uses applicative order, the arguments are
observed first. This leads to a never ending loop due to the
procedure (p).
|#


