# Write Yourself a Scheme

An adaptation of the wikibook tutorial (https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours), 
made for teaching purposes at the University of Brighton.

Starting this week, we will spend several weeks working through the
case study of writing a
[Scheme](https://en.wikipedia.org/wiki/Scheme_(programming_language))
interpreter with [Parsec](https://hackage.haskell.org/package/parsec).

Clone the repository and build it:

```
$ git clone https://github.com/jimburton/write-yourself-a-scheme
$ cd write-yourself-a-scheme
```

Start by reading the code in the module `src/WYAS0.hs`. There are
three executable cabal targets, `week1`, `week2` and `week3`. The version 
of the program in `week1` can read an expression from the command line
and understands some basic arithmetic:


```
$ cabal run week1 '(mod 12 7)'
Preprocessing executable 'week1' for write-yourself-a-scheme-0.1.0.0..
Building executable 'week1' for write-yourself-a-scheme-0.1.0.0..
Running week1...
5
```

The version of the program in `week2` starts a REPL. It knows logical
operators, functions for manipulating lists, comparing numbers and
strings:

```
$ cabal run week2
Up to date
Lisp>>> (cons 0 (car (cdr '(1 2 3))))
(0 . 2)
Lisp>>> (|| (/= 1 1) (> 2 1))
#t
Lisp>>> (string>? "hi" "how are you")
#f

```

The version of the program in `week3` allows the user to define
functions. It can also load and evaluate files containing Scheme
programs:

```
$ cabal run week3 etc/fac.scm
Up to date
120
$ cabal run week3
Lisp>>> (define (f x y) (+ x y))
(lambda ("x" "y") ...)
Lisp>>> (f 1 2)
3
Lisp>>> (f 1 2 3)
Expected 2 args; found values 1 2 3
Lisp>>> (define (factorial x) (if (= x 1) 1 (* x (factorial (- x 1)))))
(lambda ("x") ...)
Lisp>>> (factorial 10)
3628800
Lisp>>> (define (counter inc) (lambda (x) (set! inc (+ x inc)) inc))
(lambda ("inc") ...)
Lisp>>> (define my-count (counter 5))
(lambda ("x") ...)
Lisp>>> (my-count 3)
8
Lisp>>> (my-count 6)
14

```

The exercises below are a selection of those that appear at the end of
each chapter of the wikibook.

## Week 1

After the first lecture in this little series we will have reached the
end of the chapter [Evaluation, Part
1](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Evaluation,_Part_1).

+ Add support for the
  [backquote](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html#%_sec_4.2.6)
  syntactic sugar: the Scheme standard details what it should expand
  into (quasiquote/unquote).

+ Add support for
  [vectors](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.3.6). The
  Haskell representation should use
  [Vector](https://hackage.haskell.org/package/vector-0.12.3.1/docs/Data-Vector.html).

+ Add primitives to perform the various
  [type-testing](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.3)
  functions fron the Scheme standard: `symbol?`, `string?`, `number?`, etc.

+ Add the
  [symbol-handling](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.3.3)
  functions from the standard. A symbol is what we've been calling an `Atom`
  in our data constructors.
  
## Week 2

After the second lecture we will have reached the end of the chapter
[Building a
REPL](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Building_a_REPL).


```
$ cabal run week2
Preprocessing executable 'week2' for write-yourself-a-scheme-0.1.0.0..
Building executable 'week2' for write-yourself-a-scheme-0.1.0.0..
Running week2...
Lisp>>> (+ 1 2)
3
```

+ Instead of treating any non-false value as true, change the
  definition of `if` so that the predicate accepts only `Bool` values and
  throws an error on any others.
  
+ Implement the
  [cond](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html#%_idx_106)
  and
  [case](http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html#%_idx_114)
  expressions.
  
## Week 3

Carry on with the exercises from previous weeks.
