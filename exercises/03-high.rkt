#lang racket

; Scheme/Racket is a functional programming language
; As such, functions are "first-class citizens", aka
; they are values, just like everything else.


; We can easily take a function as an argument.
; This is called a "higher-order" function.
(define (apply-twice f x)
  (f (f x)))

(define (succ n) (+ n 1))

;(== (apply-twice succ 2) 4)

; Let's apply a function n times. This is actually quite useful.
(define (iterate n f x)
 (if (= n 0)
     x
     (f (iterate (- n 1) f x))))

;(iterate 10 succ 0) == 10

; We can easily define the function that we previously had to write recursively
; using higher-order functions.
(define (my-plus x y)
  (iterate x succ y))

; We can even write my-mult, by using a helper function,
; that adds x to it's argument, and then passing this helper to iterate,
; to iterate it y times, in essence multiplying x by y.
(define (my-mult-iterate x y)
  (define (help n)
    (my-plus x n))
  (iterate y help 0))


; Let's generalise the sum-interval and prod-interval
; functions that we previously wrote, so tha we instead
; apply a function to all the numbers in an interval.
; i.e.: (+ 1 (+ 2 (+ 3 0))) generalizes to
;       (oper 1 (oper 2 (oper 3 base)))
; We also need a base, because at the very least ; in sum and in prod we have different terminating values.

; We generalise this further by considering the fact that we
; might want to also apply another function to every number
; before consuming the number with our oper. (that's what the f is for)
(define (oper-interval f base oper a b)
  (if (> a b)
      base
      (oper (f a) (oper-interval f base oper (succ a) b))))

; This is all very good but it's also very annoying if we have to
; always explicitly define simple functions such as "square" beforehand.

; This is why we have lambda functions.
; They are functions that aren't bound to any symbol/identifier,
; but still work as functions. The syntax is very similar to a define expression,
; that defines a function.
; (lambda (arg1 arg2 ... argn) body-expr)
;(lambda (x) x) ; id
((lambda (x) x) 1) ; evaluates to 1

; In fact the define expression for a function is just syntactic sugar
; for a lambda expression.
(define pred (lambda (x) (- x 1)))
(pred 3)

; We can have a lambda function with no arguments.
; Note that this is not how functions in lambda calculus work.

; We can apply lambdas, just as we can normal functions,
; by evaluating them first

; A banal example for using our oper-interval function.
(define (sum-after-square a b)
  (oper-interval (lambda (x) (* x x)) 0 + a b))
; sum-after-square 2 5
; 2^2 + 3^2 + 4^2 + 5^2

; Using our higher-order functions.
(define (prod-interval a b) (oper-interval 1 * a b))

(define (fact n) (prod-interval 1 n))

; Let's showcase how similar multiplication and exponentiation are.
(define (my-mult x y)
  (if (= x 1)
      y
      (+ y (my-mult (- x 1) y))))

; In fact they differ by only one symbol! The '+'vs the '*'.
(define (my-expt x y) ; y^x
  (if (= x 1)
      y
      (* y (my-expt (- x 1) y))))

; Let's generalise then!
; by literally copy pasting, adding an additional argument
; and then replacing the symbol with our new argument.
; When given a function from our "function hierarchy", we will
; be able to simulate the next one.
; For example (next-oper expt x y) would be tetration on x and y.
(define (next-oper f x y)
  (if (= x 1)
      y
      (f y (next-oper f (- x 1) y))))

; But that's lame! We always have to supply x and y if we want to invoke
; next-oper. Whereas in reality we want next-oper to give us the next
; operation from the hierarchy. Sure the next operation will work
; on two numbers, but this is a higher order function, we shouldn't have to
; concern ourselves with measly numbers.
(define (my-expt1 x y) (next-oper * x y))

; We want to be able to instead do something like this.
; Much cooler.
;(define my-expt2 (next-oper-better *))

; And sure enough, we can. We just have to realise that exactly
; the same way we can return numbers from functions,
; we can also return functions from functions, using lambdas.
; After all, we did just say lambdas are values, like everything else.

; A function that takes a number n and returns a function that takes a number (m)
; and adds n to it.
(define (add-n n) (lambda (m) (+ n m)))

(define add-10 (add-n 10))

;(= (add-10 5) 15)
;(= (((add-n 10) 5) 15)

; Another example where we return a lambda.
; We want to compose two functions, that means
; we want a new function that's equal to first applying a function
; function, and then applying a second function.
; aka ((compose f g) x) == f(g(x))

(define (compose f g)
  (lambda (x) (f (g x))))

; On to our next-oper-better dreams.
; We must only take one argument, and return a lambda.
; Our lambda will take the remaining two arguments.
; The only other "aha"-moment/difference is the fact
; that in our recursive call we need to first evaluate our "next operation"
; before using it for our recursive call
; (the brackets around (next-oper-better f) in the recursive call)
(define (next-oper-better f)
  (lambda (x y)
    (if (= x 1)
        y
        (f y ((next-oper-better f) (- x 1) y)))))

; Now we can actually do this.
(define my-expt2 (next-oper-better *))
