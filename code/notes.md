# Notes

## Idea: Instrumentation of intermediate variables

It would be nice if, when we want to inspect the intermediate values
of a variable, rather than having to write code like this:

    (define (%merge-sort xs pred trace?)
      (let loop ((xs xs) (result '()))
           ...
           (if trace?
             (begin
               (display (first xs))
               (newline)
               (display (second xs))
               (newline)
               (newline)
               (loop ...))
            (loop ...))))

we could instead write code like this:

    (define (%merge-sort xs pred trace?)
      (instrumented-let loop ((xs xs) (result '()))
              ... normal algorithm ...
              ...))

In this hypothetical reality, `INSTRUMENTED-LET` watches the values of
all variables and prints their value each time through the loop.

I guess it will be implemented as syntax, but we'll see.

Also, it's not clear that this is the best name for it.

Oh!  And actually, it might be better to implement this in a similar
fashion to `LOAD-MODULE`, where it just rewrites whole batches of
Scheme code to generate the output needed, rather than asking the user
to manually rewrite the text of procedures.

## How to list all bound symbols in various Schemes

+ Larceny: `oblist`
+ Chez: `oblist`
+ Gambit: ?
+ Chibi: ?
+ MIT Scheme: ?
+ Chicken: ?
+ Kawa: `environment-fold`
+ JScheme: ?

## How to load R5RS `SYNTAX-RULES` in Gambit

    > (load "~~/lib/syntax-case")

## Things to do

+ Add a pathname library?

+ Add foof-loop?  (may not be really necessary)

+ Implement X11 protocol support? (See Common Lisp CLX lib)

+ Maybe break out `LET-OPTIONALS` into a separate library?

+ Consider stealing interesting things from MIT Scheme such as
  `STAR-PARSER`

+ Add various interesting codez from
  <https://mumble.net/~campbell/scheme/>

+ Implement "library aliases", so you can load SRFI-69 code by typing
  e.g. `(load-module 'hash-table)`

## Supported Schemes

### Larceny Scheme 0.99

+ uuid
+ xml
+ srfi-8
+ srfi-9
+ srfi-0
+ srfi-16
+ srfi-19
+ srfi-28
+ srfi-2
+ srfi-43
+ srfi-69
+ apropos
+ assert
+ binary-search
+ binary-tree
+ destructive-ops
+ format
+ let-optionals
+ mergesort
+ pregexp

### Chez Scheme 9.5

+ utils
+ srfi-9
+ srfi-0
+ srfi-2
+ apropos
+ assert
+ binary-search
+ binary-tree
+ destructive-ops
+ format
+ let-optionals
+ mergesort
+ pregexp
+ srfi-16
+ srfi-19
+ srfi-28
+ srfi-8

### JScheme 7.2

Note: You must load support for R5RS `define-syntax` into JScheme as
follows before you can use `LOAD-MODULE`.

    (use-module "elf/basic.scm")
    (use-module "elf/mbe.scm")

+ pregexp
+ destructive-ops
+ utils
+ assert
+ format

### Scheme 48 1.9.2

+ utils
+ apropos
+ assert
+ format
+ binary-search
+ binary-tree
+ destructive-ops
+ let-optionals
+ mergesort
+ pregexp
+ srfi-0
+ srfi-16
+ srfi-28
+ srfi-2
+ srfi-43
+ srfi-69
+ srfi-8
+ srfi-9
+ utils
+ uuid
+ xml

### Kawa 3.0 (git describe: 3.0-0-g39797ea94)
