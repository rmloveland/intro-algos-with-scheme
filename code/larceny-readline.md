# Larceny `read-line` behavior vs. Chez vs. Scheme48

There seems to be some difference in the way the same user implementation of `READ-LINE` entered at the top-level behaves in Larceny 1.3 vs. Chez 9.5 vs. Scheme48 1.9.

In Larceny, if I use the built-in `READ-LINE`, the following code works as expected:

```
(define (map-lines file)
  (call-with-input-file file
    (lambda (port)
      (let loop ((line (read-line port))
                 (lines '()))
        (if (eof-object? line)
            (reverse lines)
            (loop (read-line port)
                  (cons line lines)))))))
```

```
> (map-lines "awk-data.txt")
("Beth\t4.00\t0"
 "Dan\t\t3.75\t0"
 "Kathy\t4.00\t10"
 "Mark\t5.00\t20"
 "Mary\t5.50\t22"
 "Susie\t4.25\t18")
```

If I enter the following implementation of `READ-LINE` at the Larceny top-level, the `MAP-LINES` output changes -- a new empty string is added in between each line.

```
(define (read-line port)
  ;; Port -> String
  (let ((sep #\newline))
    (let loop ((chars '())
               (this-char (read-char port))
               (next-char (peek-char port)))
      (cond
       ((eof-object? next-char) next-char)
       ((char=? next-char sep) (list->string (reverse chars)))
       (else (loop (cons this-char chars)
                   (read-char port)
                   (peek-char port)))))))
```

```
> (map-lines "awk-data.txt")
("Beth\t4.00\t"
 ""
 "Dan\t\t3.75\t"
 ""
 "Kathy\t4.00\t1"
 ""
 "Mark\t5.00\t2"
 ""
 "Mary\t5.50\t2"
 ""
 "Susie\t4.25\t1"
 "")
```

If I switch to Chez 9.5, which apparently does not export its own version of `READ-LINE` to the top-level by default, and use the above user implementations of `READ-LINE` and `MAP-LINES`, it works as expected:

```
> (map-lines "awk-data.txt")
("Beth\t4.00\t0"
"Dan\t\t3.75\t0"
"Kathy\t4.00\t10"
"Mark\t5.00\t20" 
"Mary\t5.50\t22" 
"Susie\t4.25\t18")
```

In Scheme 48 1.9, which also does not export its own version of `READ-LINE` to the top-level by default, the user implementations of `READ-LINES` and `MAP-LINES` behave as expected (similarly to Chez, and differently from Larceny):

```
> (map-lines "awk-data.txt")
("Beth\t4.00\t0"
"Dan\t\t3.75\t0"
"Kathy\t4.00\t10"
"Mark\t5.00\t20" 
"Mary\t5.50\t22" 
"Susie\t4.25\t18")
```
