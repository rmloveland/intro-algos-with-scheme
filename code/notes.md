# notes

## instrumentation of intermediate variables

It would be nice if, when we want to inspect the intermediate values of a variable, rather than having to write code like this:

```
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
```

we could instead write code like this:

```
(define (%merge-sort xs pred trace?)
  (instrumented-let loop ((xs xs) (result '()))
          ... normal algorithm ...
          ...))
```

`INSTRUMENTED-LET` watches the values of all variables and prints their value each time through the loop.

I guess it will be implemented as syntax, but we'll see.

Also, it's not clear that this is the best name for it.

Oh!  And actually, it might be better to implement this in a similar fashion to `LOAD-MODULE`, where it just rewrites whole batches of Scheme code to generate the output needed, rather than having to manually rewrite the text of procedures.

## how to list all bound symbols

+ Larceny, Chez: `(OBLIST)`
+ Gambit: ?
+ Chibi: ?
+ MIT Scheme: ?
+ Chicken: ?

## how to load `SYNTAX-RULES` in Gambit

    > (load "~~/lib/syntax-case")

## TODO

Libraries to (maybe) add:

+ pathname library
+ foof-loop?
+ X break out `LET-OPTIONALS` into a separate library
+ anything interesting in MIT Scheme?  Perhaps `STAR-PARSER`?
+ mine some interesting code from <https://mumble.net/~campbell/scheme/>
+ implement library "aliases", so you can load SRFI-69 code by (say) `(load-module 'hash-table)`

## SUPPORTED SCHEMES

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

Note: You must load `define-syntax` into JScheme as follows before loading modules.

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
