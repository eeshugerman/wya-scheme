# Write Yourself a Scheme

## What is it?
This is my implementation of the Scheme programming language, based on a book called [Write Yourself a Scheme in 48 Hours](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours).

It's taking a lot longer than 48 hours. Is it really supposed to take 48 hours?!?

If you're here because you're working through the book yourself and are looking for a reference implementation, this may or may not be the one you want. It _is_ essentially in line with the book, but most code snippets are rewritten in my own words, and I take significant detours in the direction of completeness/compliance with the R<sup>7</sup>RS (which is not achieved by a long shot).


## Build and run it
[Stack](https://docs.haskellstack.org/en/stable/README/) is used for dependency and build management. I usually use it like so:

```bash
cd wya-scheme
stack run                # for a repl
stack run some-code.scm  # execute a file
```

## Notable Features
- full numeric tower (integer, rational, real, complex)
- `quote` (duh), `quasiquote`, `unquote`, `unquote-splicing`
- `define-macro`
- exceptions (`raise`, `with-exception-handler`)

## Notable Missing Features
- exact/inexact numbers
- continuations
- user-defined exceptions
- `define-syntax`-style macros
- libraries
- first class environments
- parameter objects
- records
- vectors
- bytevectors
- numerous derived procedures and forms
- more than a few primatives, probably
