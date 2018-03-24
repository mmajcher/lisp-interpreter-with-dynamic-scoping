## LISP interpreter

This is a clojure-exercise fun project.

An attempt to construct a simple LISP interpreter.

### How to start the REPL

Using Leiningen: `lein run` (you need both Clojure and Leiningen installed)

## Language

### Syntax

At the moment, REPL only accepts statements contained within one line.

Interpreter accepts following syntax:

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



## TODO

- ~~control flow forms~~
- multi-line input in REPL
- own parser
