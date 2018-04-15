## LISP interpreter

This is a clojure-exercise fun project.

An attempt to construct a simple LISP interpreter.

### How to start the REPL

~~Using Leiningen: `lein run` (you need both Clojure and Leiningen installed)~~

1. Download standalone jar file (^ use _release_ link above).
2. `java -jar interpreter-standalone.jar` for REPL

Jar file accepts filename argument to run code from file. The value of
evaluation of the last statement in the file will be printed out.

## Language

### Syntax

REPL accepts multi-line statements. In last line of the
statement, any input after top-level enclosing statement will be
truncated.


Here are some examples of the syntax:

```
(+ 2 2)

(define x 3)

(define square (lambda (x) (* x x)))

(let ((a 3) (b 5)) (+ a b))
```

### Dynamic scoping

This interpreter does dynamic scoping. When evaluating variable (a
free variable), it uses the value from last (in terms of call stack)
definition of the variable.

```
;; lexical scoping
(define x 3)

(define print-x (lamdba () x))

(let ((x 5)) print-x)
=> 3

;; dynamic scoping
(define x 3)

(define print-x (lambda () x))

(let ((x 5)) print-x)
=> 5


;; dynamic scoping fun
(define square (lambda (x) (* x x)))
(let ((* +)) (square 3))
=> 6
```

### Language elements

- variable evaluation

  `var-name`
- procedure application

  `(proc-name arg1 arg2 arg3 ...)`
- built-in primitives:

  `+ - / *`
- variable definition

  `(define var-name var-value)`
- procedure definition

  `(define proc-name (lambda (arg1 arg2) (+ arg1 arg2)))`
- let special form

  `(let ((binding1 val1) (binding2 val2)) body)`
- quoting / unquoting

  `(unquote '(+ 2 2))`
- if

  `(if (> x y) true-val false-val)`
- built-in predicates:

  `= not > < >= <=`
  
- begin 

  `(begin statements)`


## TODO

- ~~control flow forms~~
- ~~multi-line input in REPL~~
- own parser
