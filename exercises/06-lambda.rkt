#lang racket

; Lambda calculus is a framework for doing text processing.
; By assigning some meaning to our text processing we can create a computational model.
; (initially lambda calculus was devised as a system for logic, but it turned out to be contradictory)

; Our lambda terms are a set of symbols.
; First we have an infinite set of variables - Var = {x0,x1,x2...y,z...}

; Then:
; Recursive definition - A lambda term is:
; 1) Every x that is a variable is a lambda term.
; 2) If M and N are lambda terms, then "MN" (concatenated together) are a lambda term.
; 3) If x is a variable and M is a lambda term, then "λx.M" is a lambda term
; note that these are all strings!

; We are not going to give formal semantics to these things, as they are
; slightly more complicated than what we need.
; So have informal semantics:

; Our third clause corresponds to building functions
; λx.M is a function taking one argument and then returning M (x may or may not be present in M)
; In scheme syntax this is (lambda (x) M)

; Our second clause - MN, is pretty much function application.
; When building MN we want to think of "applying M to N".
; In scheme syntax this is just (M N)

; So for example if we assume we have numbers and a lambda term "plus"
; λx.(plus x 1) would be function that takes one argument and returns that argument + 1
; λx.(plus x 1) 2 => (plus 2 1) => 3
; This is called beta reduction. We are simply substituting the arguments to the function
; in the places they are passed.

; We want to show that we CAN actually express everything that is computable
; (aka everything that you can write with a turing machine/<your favourite programming language>

; This is out of scope right now, so let's show some simple things, to show you that stuff like this
; is actually possible.

; For example let's express one of the simplest things possibly - booleans.

; We want to think of some lambda term that corresponds to our idea of "truth" and one to our idea of "falsity"
; And ofcourse, we need to be able to use these booleans - we also need a lambda term that corresponds to an "if" construction"

; Our "truth" t is simply a function that takes two arguments and returns the first.
;λx.λy.x
(define t (lambda (x) (lambda (y) x)))

; Our "falsity" f is simply a function that takes two arguments and returns the second.
;λx.λy.y
(define f (lambda (x) (lambda (y) y)))

; How do we actually know these are useful as "true" and "false"?
; By writing an if term - (ifl b x y) that works with them, by selecting the first or the second
; branch, by getting passed respectively true or false.

; But this is simply application of the "boolean" to x and y, because of the way we built t and f!
;λb.λx.λy.(b x y)
; So if we do
;(λb.λx.λy.(b x y)) t => λx.λy.(t x y) => λx.λy.((λx.λy.x) x y) => λx.λy.x
; And we selected our first "branch", just as we desired.
(define ifl (lambda (b) (lambda (x) (lambda (y)
                                     ((b x) y)))))

; To further convince you, let's define functions to go to and from scheme bools to lambda bools

; We simply check whether we have #t or #f and return the corresponding lambda constant.
(define (schemebool->lambda x)
  (if x t f))

; We need to write a lot of brackets because our lambda functions each take only *1* argument.
; Here we do the same "check", but in reverse.
(define (lambdabool->scheme x)
  (((ifl x) #t) #f))

; After seeing we have bools, let's do the next most important thing - natural numbers.

; We will represent a natural number n as n-times application of a function to a variable.
; This is in-line with how natural numbers are usually defined:

; 1) The constant zero (0) is a natural number.
; 2) If n is a natural number, then s(n) (successor, +1) is also a natural number.

; So our zero is our function applied zero times to a variable:
;λs.λz.z
; Note how this is simply our f from earlier!
(define c0 (lambda (s) (lambda (z) z)))

; One would be a function applied a single time
;λs.λz.(s z)
(define c1 (lambda (s) (lambda (z) (s z))))

; Two..
;λs.λz.(s (s z))
; We need the brackets, because application is usually left-associative.
(define c2 (lambda (s) (lambda (z) (s (s z)))))

; What is the most basic thing we want to do with natural numbers? Well increase them by one, ofcourse;
; So let's write our successor function.
; The basic idea is this: we abuse the fact that our numbers are n-times application.
; So if we want to go from n times application to n+1 times application, we simply need to add one more
; application of our function (s), after we apply it n times.
; First we take a natural number n, and then we construct our new number, with one more s attached to it.
;λn.λs.λz.(s (n s z))
; So for example:
; (λn.λs.λz.(s (n s z))) c0 => λs.λz.(s (c0 s z)) => λs.λz.(s ((λs.λz.z) s z)) => λs.λz.(s z)
; which is exactly our constant c1!
(define cs (lambda (n) (lambda (s) (lambda (z)
                                     (s n)))))

; Let's define a scheme function that is successor, we're going to need it to go to and from scheme naturals and lambda naturals.
(define (succ n) (+ 1 n))
(define zero 0)

; This is simple recursion, we also use our cs combinator from earlier.
(define (schemenum->lambdanum x)
  (if (= x 0)
      c0
      (cs (schemenum->lambdanum (- x 1)))))

; In the opposite direction, because of the way we constructed our natural n as n-times application of a function to a variable
; it's good enough to simply pass it succ and zero. It will apply succ (our scheme successor) to zero (our scheme 0) n times,
; which will give us exactly n as a scheme number.
(define (lambdanum->schemenum x)
  ((x succ) zero))
