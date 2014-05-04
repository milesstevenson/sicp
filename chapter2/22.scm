(define (square-list1 items)
  ;; This procedure returns the list in reversed order because it
  ;; uses an iterative approach. Recursion is necessary to return
  ;; a given list in its exact order.
  ;;
  ;; In a recursive procedure (square (car things)) can be used
  ;; as the first argument to cons because it will be held on to
  ;; and joined with the rest of the new list only once (cdr things)
  ;; has been computed. Below is what answer will look like. 
  ;;
  ;;        ____(car things)__    ______________(cdr things)_________ 
  ;; (cons (square (car things)) (cons (square (car things)) ... '())))
  ;;                                                           ^ last element
  ;;                                                             of things
  ;;
  ;; In a iterative procedure this is not so. (square (car things))
  ;; is consed immediately to the new list instead of waiting for each
  ;; element of (cdr things) to be computed and consed to the new list.
  ;; Below is what answer will look like.
  ;;
  ;;          ___(cdr things)__      ______(car things)____________
  ;;   (cons (square cdr things) ... (cons (square (car things)) '()))
  ;;          ^
  ;;           last element of things
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items '()))

(define (square-list2 items)
  ;; Swapping out the order of the parameters to cons still does not yield
  ;; a list in the order that items was given. Instead we will be given a
  ;; data structure not of the list type at all, but of cons, where the car
  ;; value of the new list will always be null.
  ;;
  ;; (cons (cons '() (square (car things))) ... (square (cdr things)))
  ;;                                                     ^ last element of things
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items '()))