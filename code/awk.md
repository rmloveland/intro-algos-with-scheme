# awk in scheme

## overview

Awk is a useful mini-language for processing files, often with columnar data.

We can import the useful parts of this mini-language into Scheme.

## requirements

+ pattern-action pairs
+ record vars ($0, $1, $2, etc.)
+ field vars ($NF, $NR, etc.)
+ BEGIN and END blocks
