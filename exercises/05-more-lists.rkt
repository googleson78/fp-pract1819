#lang racket

; let's remind ourselves what foldr looks like
; it generates a "right-leaning" tree (that's why it's called fold right)
; if you write out the expression tree for a foldr application

; it's also a recursive function, in general, so we can't have the
; efficient tail recursion optimisation, which is not good.

; can we still have an abstract operation, that is tail-recursive
(define (my-foldr f v xs)
  (if (null? xs)
      v
      (f (car xs) (my-foldr f v (cdr xs)))))

(define (sum xs)
  (foldr + 0 xs))

; foldr is always not the most convenient, due to the way the parentheses are placed
; if we were to try to implement this function with foldr the naive way
; we would get constantly flipping - signs, which is totally not what we want
; (- 1 (- 2 (- 3 10))), instead of (- (- (- 10 1) 2) 3)
(define (minus-from n xs)
  (if (null? xs)
      n
      (minus-from (- n (car xs)) (cdr xs))))

; same for this function
(define (divide-by n xs)
  (if (null? xs)
      n
      (divide-by (/ n (car xs)) (cdr xs))))

; let's also look at our efficient tail-recursive sum function
(define (sum-iter xs)
  (define (helper acc ys)
    (if (null? ys)
        acc
        (helper (+ acc (car ys)) (cdr ys))))
  (helper 0 xs))

; let's write a tail-recursive reverse
; we need a helper to keep track of what we have reversed so far
; and to allow us to append "at the back" of something
; this way if we already have a reversed list we can just keep
; consuming items from the front of our initial list, and since we will cons them
; in the order we have consumed them, this will in turn flip their order
(define (my-reverse xs)
  (define (helper reversed ys)
    (if (null? ys)
        reversed
        (helper (cons (car ys) reversed) (cdr ys))))
  (helper '() xs))

; from these functions that we saw so far we can almost automatically
; derive a new type of fold - one that is
; a) tail-recusrive
; b) allows us to work with "brackets applied on the left"
; the key insight here is that instead of always carrying around a null value
; like in our foldr function, and only using it at the end, we can instead
; use it as a sort of accumulator, like the ones we have in our helper functions

(define (foldl f nv xs)
  (if (null? xs)
      nv
      (foldl f (f nv (car xs)) (cdr xs))))

; now we can easily define our minus-from with correct brackets
(define (minus-from-foldl n xs)
  (foldl - n xs))

; but wait woah, reverse's helper is also very similar to our foldl
; let's try rewriting reverse with foldl
(define (reverse-foldl-wrong xs)
  (foldl cons '() xs))

; awesome. and from
(reverse-foldl-wrong '(1 2 3))

; we get
'(((() . 1) . 2) . 3)

; ..wait what?

; the issue here stems from the order of arguments in our foldl operation
; and in cons; in foldl we pass the accumulator/recursive call as the first argument
; and the head as the second argument, whereas in cons we need to give
; our current item as the first argument and the recursive call as the second

; in essence, we can either rewrite foldl (which is not good, as this is
; the standard foldl operation)
; or we can either simply flip the operands of our cons

; yay!..
(define (flipped-cons x y) (cons y x))
; .. more like nay!

; we can do better!
; let's just instead flip the arguments of an arbitrary 2-placed function
(define (flip f)
  (lambda (x y) (f y x)))

; yay, abstraction! (flip is actually pretty useful, I'm not making stuff up)
; now we can easily write our reverse
(define (reverse-foldl xs)
  (foldl (flip cons) '() xs))
; which now works correctly

; now for some arbitrary list functions
; let's "zip together" two lists
; i.e. make a list of pairwise the elements of the two lists
; if one of the lists is shorter we truncate the result to the shorter lists' length
; examples:  (zip '(1 2 3) '(4 5 6))     -> '((1 . 4) (2 . 5) (3 . 6))
;            (zip '(1 2)   '(4 5 6 7 8)) -> '((1 . 4) (2 . 5))

; we would love to use a fold here, but plain old recursion is easier here
; since we have to recurse on two things at once, and our folds, at their core
; work on one list at a time
(define (zip xs ys)
  (if (or (null? xs) (null? ys))
      '()
      (cons (cons (car xs) (car ys)) (zip (cdr xs) (cdr ys)))))

; let's do a more general zip
; instead of always constructing 2-tuples, we will apply a function
; pairwise to the elements of a list
; example: (zip-with + '(1 1 1) '(2 3 4)) -> '(3 4 5)

(define (zip-with f xs ys)
  (if (or (null? xs) (null? ys))
      '()
      (cons (f (car xs) (car ys)) (zip-with f (cdr xs) (cdr ys)))))

; let's do a dot product to show that this is actually useful
(define (dotp xs ys)
  (sum (zip-with * xs ys)))

; now, let's do a very important list function
; we take a list of lists, and flatten them to one list
; example: (concat '((1 2 3) (4 5 6) (7 8 9))) -> '(1 2 3 4 5 6 7 8 9)
; note: append is built-in, but you can also easily write it ( (foldr cons ys xs) )
(define (concat xss)
  (if (null? xss)
      '()
      (append (car xss) (concat (cdr xss)))))

; ...wait, haven't I seen this pattern somewhere before??

(define (concat-better xss)
  (foldr append '() xss))

; but that's recursive!
; but append is also an associative operation - it means that we can
; "place the brackets however we want", in other words, for it
; foldl and foldr are equivalent (when using '() as our null-value)
(define (concat-best xss)
  (foldl append '() xss))

; let's do a cartesian product, now that we've seen a zip
; zip was pairwise element cons-ing, this is "each with each"
; i.e. for each x in xs we cons x with each y in ys
; example : (cartesian '(1 2) '(4 5)) -> '((1 . 4) (1 . 5) (2 . 4) (2 . 5))

; to translate what we just said into code it's easier to define a helper
; that cons-es one x with all the ys

; note how I said "for all", whenever you have an operation that sounds like
; "for all" something, or when you would normally use a foreach loop in an imperative
; language, you would want to use a map here, and you can even see how cool it looks

; not only can we define our helper with a map, but then we can just map our helper
; for each x !!!! which results in a very clean implementation

; the only thing we have to watch out for is the fact that in the end
; we end up with a list of lists of tuples, instead of a list of tuples
; hm.. we just implemented a function to help with that - concat
(define (cartesian xs ys)
  (define (cons-one x)
    (map (lambda (y) (cons x y)) ys))

  (concat (map cons-one xs)))

; let's do concat, but instead recursively down for all lists
; we want a function that given anything, sets it's "list depth" to 1
; to do so we need to consider three cases
; 1) the empty list - we already have a list with depth 1
; 2) a non-empty list - we need to flatten all of it's items
;                       and then append them together (our recursive step)
; 3) something that isn't a list - we need to add it to a singleton,
;                                  so that it's depth becomes 1 from 0

; example: (flatten '((1 2 3) 4 () ((5 (6)) 7) (((8))))) -> '(1 2 3 4 5 6 7 8)
(define (flatten xss)
  (cond ((null? xss) '())
        ((list? xss) (concat (map flatten xss)))
        (else (cons xss '()))))

; finally something that we discussed only briefly with some of you (so I guess we didn't do this in class)
; "deep reversal" - we are reversing lists, like normal, but this time
; we also want to recursively reverse all the sub-lists
; we break-down our steps in a similar way to the deep flattening

; example: (uber-reverse '((1 2 3) 4 () ((5 (6)) 7) (((8))))) -> '((((8))) (7 ((6) 5)) () 4 (3 2 1))
(define (uber-reverse xs)
  (cond ((null? xs) '())
        ((pair? xs) (reverse (map uber-reverse xs)))
        (else       xs)))
