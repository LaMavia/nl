# Not Lisp

## Syntax

The syntax is constructed around lisp-like constructions:

```
expression      := bare_expression | '(' bare_expression ')' | '(' ')'
bare_expression := token expression | token
token           := keyword | identifier | operator | 

```

### Keywords

#### let

```
let :: identifier -> expression -> expression

(let x 5 (+ x 3)) = (+ 5 3) = 8
( let x 5 (
    let x 7 (
      * 2 x
    )
  )
) = (let x 7 (* 2 x)) = (* 2 7) = 14
```

### Functions
```
( let add (λ (x, y) (+ x y))
  ( add 1 2 )
)
```

```
λ :: list[identifier,] -> expression

(λ (x, y) (- x y)) 5 2 
  = (λ x (λ y (- x y))) 5 2
  = (λ y (- 5 y)) 2
  = (- 5 2)
  = 3
```
