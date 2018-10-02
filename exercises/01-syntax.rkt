; we need to specify this line, for some more complicated reasons
; than we are currently interested in
#lang racket
; A one line comment

; everything is a value
; integers
1
; negative numbers
-1
; fractional numbers
3/4
; floats
3.14
; strings
"asdf"
; bools
#t
#f
; even functions
+

; function application - the first item in the brackets
; is the function, and the rest are it's arguments
(+ 1 2)
; + and * are special - they take varying numbers of arguments
(* 3 4 5)
(* 3 4 5 6)

; we can easily nest function calls by replacing one of the arguments
; with another function call (which is itself a value still)
; we also put our arguments on new-lines, aligned to each other
(*
  (+ 1 2)
  (- 5 4))

; we can define names to bind them to some values
(define a 42)

; we can even bind functions
(define p +)
(p 33 36)

; we can define functions by specifying which arguments they take
; and then specifying their body
; the syntax is (define (func-name arg1 arg2 arg3..) function-body)
; where function-body is a value

; a function that takes two arguments and calculates their sum
(define (sum-two x y) (+ x y))

; a function that takes an argument and finds it's successor
(define (succ x) (+ x 1))
(define (pred x) (- x 1))

; we have conditional statements in the form of an if-then-else clause
; which is again itself a value
; syntax - (if cond then-clause else-clause), where all three of
; cond, then-clause, else-clause are values, and cond is a boolean value

; = is used to compare integers for equality
(if (= 3 3) 4 5)

; Let's define our own addition and multiplication using succ and pred, as an exercise in syntax and recursion
; Only for natural numbers. Using the following (mathematical) definition:
; my-plus(0, y) = y
; my-plus(x + 1, y) = succ(my-plus(x, y))
(define (my-plus x y)
  (if (= x 0)
      y
      (succ (my-plus (pred x) y))))

(my-plus 10 15)

; Now for multiplication using my-plus, using a similar definition:
; my-mult(0, y) = 0
; my-mult(1, y) = y
; my-mult(x + 1, y) = y + my-mult(x, y)
(define (my-mult x y)
  (if (= x 0)
      0
      (if (= x 1)
          y
          (+ y (my-mult (pred x) y)))))

(my-mult 12 12)

; How about fast-pow?:
; fast-pow(x, 0) = 1
; fast-pow(x, 2*n) = fast-pow(x * x, n)
; fast-pow(x, 2*n + 1) = x * fast-pow(x * x, n)
(define (fast-pow x n)
  (if (= n 0)
      1
      (if (= (remainder n 2) 0)
          (fast-pow (my-mult x x) (quotient n 2))
          (*
            x
            (fast-pow (my-mult x x) (quotient n 2))))))

(fast-pow 2 10)

; We can also nest definitions to use as helpers.
; They can be used in the scope in which they are defined
; (just as we defined stuff in the global scope, and used it in the global scope until now)
; For example is a number prime:
; To find out if a number is prime we just check all numbers between 2 and the number itself
; keeping track if any of them divide the number, using an and (&&) operation.

; predicates (functions that return true/false) conventionally end in an ?
(define (prime? n)
  ; Does x divide y?
  (define (divides x y)
    (= 0 (remainder y x)))

  ; A helper function, to "iterate" through all the numbers from 2 to n
  ; i is our "counter". If we encounter a number that divides n, we return false.
  ; Otherwise we need to increment the counter and try again. Reaching n means we're done checking,
  ; and our number passed the test. This can alternatively be written using an and (&&) operation.
  (define (help i)
    (if (= i n)
        #t
        (if (divides i n)
            #f
            (help (+ i 1)))))

  ; Finally we need to start from somewhere, to avoid instantly returning a false negative because of 1
  ; we start from 2.
  (help 2))

(prime? 5)
(prime? 69)
