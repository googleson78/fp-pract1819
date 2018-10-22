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

; lists

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
(define (sum xs) void)

; Let's write a function that when given an interval
; gives us all the integers in the interval, as a list.
(define (fromTo a b) '())

(equal? (fromTo 1 5) '(1 2 3 4 5))

(= (sum (fromTo 1 100)) 5050)

; Let's write a function that does the same as sum,
; but with product instead.

(define (prod xs) void)

; Let's define factorial using it and fromTo.

(define (fact n) void)

; sum/product similarity
; foldr
; sum/product using foldr

; append
; append with foldr

; map
; map with foldr

; filter
; filter with foldr

; foldl
; foldl with foldr
