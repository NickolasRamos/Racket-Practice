#lang racket
;Nick Ramos
;CS4337.0U1

(provide divisible-by-7?)
(provide function-3)
(provide my-map)
(provide zipper)
(provide segregate)
(provide is-member?)
(provide my-sorted?)
(provide my-flatten)
(provide upper-threshold)
(provide my-list-ref)
(provide deep-reverse)


;1) Takes a single integer as an argument and returns a Boolean
;that indicates whether the number is evenly divisible by 7.
(define (divisible-by-7? input)
  (zero?(modulo input 7)))

;2) Takes a function as an argument and passes the number 3 to
;that function.
(define (function-3 input)(input 3))

;3) Duplicates the functionality of map from the standard library.
(define (my-map func list)
  (cond ((empty? list) null)
        (else(cons (func (first list))
                   (my-map func (rest list))))))

;4) Takes two lists as arguments and returns a single list of pairs.
(define (zipper list1 list2)
  (if (or (empty? list1) (empty? list2))`()
      (cons (list (first list1) (first list2))
            (zipper (rest list1)(rest list2)))))

;5) Takes a list of integers  as an argument and returns a list containing
;two sublists, the first sublist containing the even numbers from the
;original list and the second sublist containing the odd numbers from the
;original lists.
(define (segregate list1)
  (foldr (λ (en on)
           (if (even? en)
               (list (cons en (first on)) (second on))
               (list (first on) (cons en (second on)))))
           `(()()) list1))

;6) Takes two arguments, a list and a single element. Returns true if the
;element is a member of the list and false if it does not.
(define (is-member? num list1)
  (cond ((empty? list1) #f)
        ((equal? num (first list1)) #t)
        (else (is-member? num (rest list1)))))

;7) Takes a list as an argument. Returns a boolean indicating whether the list
;is sorted in ascedning order.
(define (my-sorted? list1)
  (if (number? (car list1))
      (if (<= (length list1) 1)
          #t
          (and (<= (car list1) (cadr list1)) (my-sorted? (cdr list1)) ))
      (if (<= (length list1) 1)
          #t
          (and (string<=? (car list1) (cadr list1))(my-sorted? (cdr list1)) ))))

;8) Duplicates the functionality of flatten from the standard library.
(define (my-flatten list1)
  (cond ((empty? list1) `())
        ((pair? (first list1))
         (append (my-flatten (first list1)) (my-flatten (rest list1))))
        (else (cons (car list1) (my-flatten (cdr list1))))))

;9) Takes two arguments, a list of numbers and a single number. It returns
;a new list that has the same numbers as the input list, but with all elements
;greater than the threshold number removed.
(define (upper-threshold list1 n)
  (cond ((empty? list1) `())
        ((> n (first list1)) (cons (first list1) (upper-threshold (rest list1) n)))
        (else (upper-threshold (rest list1) n))))

;10) Duplicates the functionality of list-ref from the standard library.
(define (my-list-ref list1 n)
  (if (zero? n)
      (first list1)
      (if (> n (length list1))
          (error "Error: Out of bounds")
          (my-list-ref (rest list1) (- n 1)))))

;11) Similar to the reverse function, except that it acts recursively,
;reversing the order the members of any sublists.

;Here is my-reverse
(define (my-reverse list1)
  (if (empty? list1)
      `()
      (append (my-reverse (rest list1))(list (first list1)))))

;Here is the deep reverse and it uses my-map from problem 3 and the
;my-reverse helper function
(define (deep-reverse list2)
  (if (list? list2)
      (my-reverse (my-map deep-reverse list2))
                  list2))