# Write Yourself a Scheme

## What is it?
This is my implementation of the Scheme programming language, based on a book called [*Write Yourself a Scheme in 48 Hours*](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours).

It's taking a lot longer than 48 hours. Is it really supposed to take 48 hours?!?

If you're here because you're working through the book yourself and are looking for a reference implementation, this may or may not be the one you want. It _is_ essentially in line with the book, but most code snippets are rewritten in my own words, and I take significant detours in the direction of completeness/compliance with the R<sup>7</sup>RS (which is not achieved by a long shot).


## Build and run it
[Stack](https://docs.haskellstack.org) is used for dependency and build management. I usually use it like so:

```bash
cd wya-scheme
stack run                # for a repl
stack run some-code.scm  # execute a file
```

## Notable Features
- full numeric tower (integer, rational, real, complex)
- `quote`, `quasiquote`, `unquote`, `unquote-splicing`
- `define-macro`
- exceptions (`raise`, `with-exception-handler`)
- I/O (ports)

## Notable Missing Features
- comments
- exact/inexact numbers
- continuations
- user-defined exceptions
- `define-syntax`-style macros
- TCO
- libraries
- first class environments
- parameter objects
- records
- vectors
- bytevectors
- numerous derived procedures and forms
- more than a few primatives, probably

## Confessions

### `IORef`
`IORef` is used to implement the mutable Scheme environment, because that's what the book uses. The book justifies this in the first few paragraphs of [chapter 7](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Adding_Variables_and_Assignment), in particular:

> The `IORef` module lets you use stateful variables within the IO monad. Since our state has to be interleaved with IO anyway (it persists between lines in the REPL, and we will eventually have IO functions within the language itself), we'll be using `IORef`s. 

But apparently there's a nicer way to handle this: [*Write You A Scheme, Version 2*](https://wespiser.com/writings/wyas/home.html) (by a different author) does not use `IORef`, but still provides a REPL. Instead, the Reader monad (which handles lexical scope) and IO monad are combined using monad transformers.

Overall the original book struck me as friendlier introduction to Haskell than did *Version 2*, so I stuck with it and `IORef`, but I'd still like to explore the Reader/transformer approach at some point.

### `String`
Another tidbit I learned from skimming *Version 2*: `Text` is better than `String`. Yet this project uses `String`.
