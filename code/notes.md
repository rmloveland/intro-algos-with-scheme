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

## Prelude load order

Note: This may be obsolete with the introduction of the [require](https://github.com/rmloveland/load-module/commit/f338a80e4f95dcbd13da796bc1f9187c3fa7f702) keyword.

+ srfi-0
+ srfi-8
+ srfi-9
+ srfi-14
+ srfi-13
+ srfi-16
+ srfi-43
+ srfi-69

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
+ break out `LET-OPTIONALS` into a separate library
+ anything interesting in MIT Scheme?  Perhaps `STAR-PARSER`?
+ mine some interesting code from <https://mumble.net/~campbell/scheme/>
