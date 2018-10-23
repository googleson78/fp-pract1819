#lang racket

; briefly on =, eq?, eqv?, equal?

; (= a b) works only on integers

; (eq? a b) is literal pointer comparison, are a and b the same object

; (eqv? a b) is exactly the same as (eq? a b), EXCEPT for a few types
; for example integers and characters
; mnemonic: eqv(alue)

; (equal? a b) is exactly the same as (eqv? a b), EXCEPT for a *lot* of types
; use this if you want the most "certain" equality
; accordingly it is also the slowest out of the three

; L       L
;   I   I
;     S
;   T   T
; S       S

; Everything we've been writing until now when doing
; function application has been a "list" of things, the first of which is
; usually a function.

; However using quote we can delay the evaluation of this list and treat it
; as just that.

; Let's look at 2-tuples first.
; We can make a 2-tuple using cons(truct), or using .
; However when using . we need to quote the expression, because
; (cons a b) evaluates to '(a . b), whereas (1 . 2) is nonsensical,
; since 1 isn't a function.
(equal? (cons 1 2) '(1 . 2))

; We can extract the two elments of a 2-tuple using car and cdr
; car stands for some or other register and cdr stands for some or other register
(= (car (cons 1 2)) 1)
(= (cdr (cons 1 2)) 2)

; Armed with this awesome power let's define what a list is. (recursively)
; 1) '() is a list
; 2) for any a, if b is a list, then (cons a b) is a list.

; Examples:
(cons 1 (cons 2 (cons 3 '())))

(cons '() (cons '() (cons '() '())))

; We can use this short-hand to write lists:
; First we quote the expression, and then we simply изброяваме items.
'(1 2 3)
'(w t f)

; We can also check whether a list is the empty list.
(null? '())
(eqv? (null? '(1)) #f)

; Let's write some functions on lists now.
; Our favourite sum-interval, but this time over a list.
; We want to sum all the elements of a list.

; We just do recursion on a list - either it's empty,
; or we can access it's head, do something with it,
; and then do something with the rest.
(define (sum xs)
  (if (null? xs)
      0
      (+ (car xs) (sum (cdr xs)))))

; Let's write a function that when given an interval
; gives us all the integers in the interval, as a list.
(define (fromTo a b)
  (if (> a b)
      '()
      (cons a (fromTo (+ a 1) b))))

(fromTo 1 10)

(equal? (fromTo 1 5) '(1 2 3 4 5))

(= (sum (fromTo 1 100)) 5050)

; We can now compose our functions to write our beloved sum-interval

(define (sum-interval a b)
  (sum (fromTo a b)))

; Let's write a function that does the same as sum,
; but with product instead.

(define (prod xs)
  (if (null? xs)
      1
      (* (car xs) (prod (cdr xs)))))

; Let's define factorial using it and fromTo.

(define (fact n)
  (prod (fromTo 1 n)))

; Let's append two lists
; We need to do our recursion on the first list,
; because we need access to it's head, in order to be able to place it
; as the head of our new list.

; N.B. Don't do recursion on two things at the same time. It's almost never necessary.
(define (my-append xs ys)
  (if (null? xs)
      ys
      (cons (car xs) (my-append (cdr xs) ys))))

; Wow! sum and prod are really similar!
; Let's abstract over their differences so that we have a general-purpose function
; which we can use to do both.

; Let's call it fold, because it's like taking a list and folding it into one object.
; (use your imagination.)

; The only differences are in our base case, when the list is empty,
; and what we do to the head of the list and our recursive call result.

; (nv stands for null value)
(define (fold f nv xs)
  (if (null? xs)
      nv
      (f (car xs) (fold f nv (cdr xs)))))
; Obviously it's really easy to express sum and prod now using fold.
; But let's do something slightly cooler - we can also express append!

(define (append-fold xs ys)
  (fold cons ys xs))

(append-fold '(1 2 3) '(3 4 5))

; In general fold is "abstract recursion" over a list.
; We handle our empty list case, and we also handle our recursive step.
; In fact, it turns out that in a language with higher-order functions
; fold is strong enough to express all computable functions.

; This is what we use instead of a for loop in general.

; And now a more restricted version, that's probably the most common thing you do,
; even if you haven't realised it - Uniformly applying a function across all the
; arguments of a list. This is called map, and you've probably heard of it in
; other languages too.
(define (my-map f xs)
  (if (null? xs)
      '()
      (cons (f (car xs)) (my-map f (cdr xs)))))

(define (sqr x) (* x x))

(equal? '(1 4 9 16) (my-map sqr '(1 2 3 4)))

; As I said fold is pretty general. Have a map implemented using fold:

(define (my-map-fold f xs)
  (fold (lambda (head tail) (cons (f head) tail)) '() xs))
(equal? (my-map-fold sqr '(1 2 3 4)) (my-map sqr '(1 2 3 4)))

; And another one - sometimes we only need elements matching a certain criteria.
; For that we have filter - we have a predicate p, and we only leave elements
; matching that predicate.

(define (filter p xs)
  (if (null? xs)
      '()
      (if (p (car xs))
          (cons (car xs) (filter p (cdr xs)))
          (filter p (cdr xs)))))

(equal? '(2) (filter even? '(1 2 3)))

; Cooler styled filter.
; Instead of choosing between two values in our if statement,
; we choose between two functions - the cons function,
; and the function which always returns it's second argument (usually called K*).
; and then apply the corresponding function to our head and recursive call.
(define (filter-cool p xs)
  (if (null? xs)
      '()
      ((if (p (car xs))
           cons
           (lambda (x y) y))
       (car xs) (filter-cool p (cdr xs)))))

(equal? (filter even? '(1 2 3)) (filter-cool even? '(1 2 3)))

; As usual let's show-off with fold.
; This is even much cleaner, in my opinion.
(define (filter-fold p xs)
  (fold (lambda (head tail) (if (p head) (cons head tail) tail))
        '()
        xs))

(filter-fold odd? '(1 2 3))

; Some other stuff we did in the spur of the moment.

; We can index lists. But it's quite ineffective, seeing as how they're linked lists.
; Don't worry about invalid values.
(define (index n xs)
  (if (= n 0)
      (car xs)
      (index (- n 1) (cdr xs))))

; We can also take a sublist between two indices.
; We have two easy ways of doing this - either iteratively with a helper
; and keeping track of what index we're at, or by breaking the problem
; down into first dropping a certain number of elements, and then taking
; a certain number of elements. I like the second one more, it's cleaner
; and more "functional".

(define (take n xs)
  (if (= 0 n)
      '()
      (cons (car xs) (take (- n 1) (cdr xs)))))

(define (drop n xs)
  (if (= 0 n)
      xs
      (drop (- n 1) (cdr xs))))

; Note that we need to only take the first end - start, because we are effectively
; shifting the "indexing" of the list to start from "start"
(define (sublist start end xs)
  (take (- end start) (drop start xs)))

(equal? '(3 4) (sublist 3 5 '(0 1 2 3 4 5 6)))
