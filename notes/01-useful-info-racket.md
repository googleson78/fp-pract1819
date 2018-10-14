# Useful Racket info
[Racket guide]

[Racket docs]

[Download Racket] (but preferably use your distro's package manager, as always)

> IDE?

Use `DrRacket`.

Or you can use the terminal `racket` interpreter,
but it's not very good (no automatic indentation or bracket matching).

## Styling
Operands are aligned with one another and go on new lines.

Feel free to ignore this if it's a one-liner, but please don't otherwise.

Examples:
```racket
(+
  1
  2)

(define f *)

(f
  1
  (+
    4
    5))
```

[Racket guide]: https://docs.racket-lang.org/guide/index.html
[Racket docs]: https://docs.racket-lang.org/reference/index.html
[Download Racket]: https://download.racket-lang.org/ (preferably your package manager)
