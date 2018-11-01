#lang racket

(provide winner
         play)

(define (id x) x)

; winner returns one of 4 possible things:
; Either the game hasn't ended, in which case it returns #f
; Or one of X or O has won, in which case it returns "X" or "O", respectively.
; Or the game has ended in a draw in which case it returns "D".

; If you are wondering how winner works, read up on
; "and" and "andmap", on docs.racket-lang.org.
; Basically everything that is not #f evaluates to #t (like in C++, for example).
; Furthermore (and "A" "B" 5) will not return #t, because they all evalute to #t,
; but will instead return the last item in the list.
; If it encounters an #f instead it will return #f.

; Only detects draws right now.
(define (winner b)
  (if (andmap (lambda (xs) (andmap id xs)) b)
      "D"
      #f))

; plays is the "ai player". It takes the current board state in curr-board,
; as a list of lists, i.e.
; '(("X" #f "O")
;   (#f "X" #f)
;   ("O" #f "X"))
; and what the sign being placed right now is, in curr-sign.

; Board legend:
; #f - empty spot
; "X" - an X resides on that spot
; "O" - an O resides on that spot

; Current sign legend:
; #t - the current char is an X (you're placing an X)
; #f - the current char is an O (you're placing an O)

; Naive "AI", plays the "next" free spot, going left-to-right, top-to-bottom.
(define (play curr-board curr-sign)
  (define (helper i j)
    (cond ((> i 2) #f)
          ((> j 2) (helper (+ i 1) 0))
          ((not (list-ref (list-ref curr-board i) j)) (cons i j))
          (else (helper i (+ j 1)))))
  (helper 0 0))
