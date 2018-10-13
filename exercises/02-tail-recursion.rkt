#lang racket

; let's first solve the task that we rushed to do
; at the end of our last class
;
; write a predicate prime? n that is true iff n is prime

; a sub-function, for ease of use
(define (divides p q)
  (= (remainder q p) 0)) ; (remainder x y) == x % y

; we will use an inner helper to iterate over the numbers
; from 2 to n and check if any of them divide n
; if we find one that does we immediately know that it's not prime
; otherwise if we reach n itself it means none of the previous ones
; divided it - meaning it's prime.
(define (prime? n)
  (define (help i)
    (if (eq? i n)
        #t
        (if (divides i n)
            #f
            (help (+ i 1)))))
  (help 2))



; let's sum the numbers in the interval [a, b]
; show off tail recursion

; recursive
(define (sum-interval a b)
  (if (= a b)
      a
      (+ a (sum-interval (+ a 1) b))))

; tail recursive, using an accumulator
; as "memory" to keep track of our current value
; and pass it along to the next recursive call
; this is equivalent in C++ to
; int sum_interval_iter(int n)
; {
;   int acc = 0;
;
;   for (int i = 0; i <= n; ++i)
;     acc += i
;
;   return acc;
; }
(define (sum-interval-iter a b)
  (define (help i acc)
    (if (= i b)
        (+ i acc)
        (help (+ i 1) (+ acc i))))
  (help a 0))


; we use (time x), a function in racket
; that allows us to see how much time our program spent calculating x
; in addition to outputting x's value
(time (sum-interval 1 10000000))
(time (sum-interval-iter 1 10000000))

; output:
;|| cpu time: 627 real time: 627 gc time: 349
;|| 50000005000000
;|| cpu time: 44 real time: 44 gc time: 0
;|| 50000005000000

; cpu time is the actual time spent calculating, gc time is how much time
; the garbage collector took up.
; as we can see our tail recursive function gets optimised
; and spends no time doing gc (because it runs in constant memory)
; it's also faster in general, as a consequence


; let's show off how inefficient recursive fibonacci is
;
; literal translation of the definition
(define (fib n)
  (cond ((= 0 n) 0)
        ((= 1 n) 1)
        (else (+
                (fib (- n 1))
                (fib (- n 2))))))

; let's show off how much better an iterative (and also tail recursive) version is
;
; we have the following invariant: for every call of help
; curr will contain the i-th fibonacci number and prev the (i-1)-th fibonacci number
; in order to implement our function we just need to make sure this stays true;
; if prev is our (i-1)-th number and curr is our i-th, obviously (+ prev curr)
; is our (i + 1)-th number, and this is reflected in our recursive call
;
; when we reach (= i n) we know that our invariant holds, so we just return the i-th
; (in other words the n-th) number.
(define (fib-iter n)
  (define (help prev curr i)
    (if (= i n)
        curr
        (help curr (+ prev curr) (+ i 1))))
  (help 0 1 1))

(time (fib      37))
(time (fib-iter 37))

; let's implement another classic - the ackermann function
;
; literal translation of the definition
(define (ack m n)
  (cond ((= 0 m) (+ n 1))
        ((= 0 n) (ack (- m 1) 1))
        (else    (ack (- m 1) (ack m (- n 1))))))

; our tail recursive functions until now have expressed an iterative process
; this cannot be done with ackermann.
; why?:
;
; the m argument is roughly how "high" in the operation hierarchy
; we're going to be - for example
; 0 ~ succession
; 1 ~ summation (not really if you try it out, but for the sake of the example)
; 2 ~ multiplication
; 3 ~ exponentiation
; 4 ~ tetration (that is, exponentiation with a "tower" of n exponents applied to one another)
; 5 ~ pentation (look both of these up if you are curious
; ..etc
;
; with this intuition we can see that it's hard (read: impossible) to express ackermann
; with an iterative process, as we can't know beforehand "how many for-loops" we are going to need
; if we have only succession as a primitive operation:
; for m = 1 - we need one loop; for m = 2 we need two nested for loops; for m = 3 we need three nested for loops
; ..etc
