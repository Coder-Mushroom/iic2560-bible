#lang plai

; ===================================== ;
;     __   _ __   __          __   __   ;
;    / /  (_) /  / /__   ____/ /__/ /_  ;
;   / _ \/ / _ \/ / -_) / __/  '_/ __/  ;
;  /_.__/_/_.__/_/\__(_)_/ /_/\_\\__/   ;
; ===================================== ;
; Desc.: A file with an interactive     ;
; summary for the programming language  ;
; fundamentals course (iic2560)         ;
; Author: Coder-Mushroom                ;
; Date: first semester 2025             ;
; ------------------------------------- ;



; ==================================== ;
;         Sec. 1: Data types           ;
; ==================================== ;

; # Primitive types
42          ; integer
3.14        ; float
1/3         ; fraction
#t          ; bool (true)
#f          ; bool (false)
"hello! ^^" ; string (modifiable)
'plai       ; symbol (unmodifiable)

; # Data structures

; ## Pairs (2-tuples)
(cons 2 4) ; output: '(2 . 4)

; ## Lists (n-tuples, same formal definition by nested pairs)
empty                            ; empty list (output: '())
(cons 1 (cons 2 (cons 3 empty))) ; output: '(1 2 3)
(list 1 2 3)                     ; equivalent to above expression

; # User-defined types
; It's possible to define your own data types.
; One of the uses for them is to represent the grammar of a language (see Sec. 6).
; The following example defines a type that represents a binary tree

; ## Definition
(define-type BinTree
  [leaf (val number?)]
  [node (val number?) (leftc BinTree?) (rightc BinTree?)]
)

; ## Instantiation
(define bin_leaf (leaf 2))
(define bin_root (node 0 (node 1 (leaf 3) (leaf 4) ) (node 2 (leaf 5) (leaf 6))))

; ## Methods

; ### For obtaining a value from the type
; Syntax: *type_name*-*property-name*
(leaf-val bin_leaf)     ; returns the value of a leaf (output: 2)
(node-val bin_root)     ; returns the value of a node (output: 0)
(node-leftc bin_root)   ; returns the left child of a node (output: (node 1 (leaf 3) (leaf 4)))
(node-rightc bin_root)  ; returns the right child of a node (output: (node 2 (leaf 5) (leaf 6)))

; ### For checking if a value is of a certain type (contract validation)
(leaf? bin_leaf)     ; checks if bin_leaf is of type leaf (returns: #t)
(leaf? bin_root)     ; checks if bin_node is of type leaf (returns: #f)
(node? bin_leaf)     ; checks if bin_leaf is of type node (returns: #f)
(node? bin_root)     ; checks if bin_node is of type node (returns: #t)
(BinTree? bin_leaf)  ; checks if bin_leaf is of type BinTree (returns: #t)
(BinTree? bin_root)  ; checks if bin_node is of type BinTree (returns: #t)

; ------------------------------------ ;



; ==================================== ;
;     Sec. 2: Built-in functions       ;
; ==================================== ;

; # General utility
(printf "Hello, plai!\n") ; print to console
(test (+ 1 2) 3)          ; test to see if output (in the ex: (+ 1 2)) matches expected value (in the ex: 3)

; # Basic math (all of the following, except for sqrt, accept N arguments)
(+ 1 2 3)    ; addition
(- 1 2 4)    ; subtraction
(* 2 3 5 -1) ; multiplication
(/ 2 3 12 5) ; division
(sqrt 2)     ; square root

; # Numerical comparison
(= 1 1)   ; equal to
(> 1 2)   ; greater than
(< 1 2)   ; less than
(>= 2 2)  ; greater or equal to
(<= 3 2)  ; less or equal to
(odd? 1)  ; is odd
(even? 1) ; is even

; # Other comparison operations
(equal? "hello" "bye") ; equal to for general use

; # Logical operations
(or #t #f)  ; logical or 
(and #t #f) ; logical and

; # For pairs and lists (all of the following, except for append, work with pairs and lists)
(empty? (cons 1 2))                ; checks if empty
(car (cons 1 2))                   ; returns first element
(cdr (list 1 2 3))                 ; returns remainder (everything to the right of the first element)
(append (list 1 2 3) (list 3 2 1)) ; joins two LISTS

; ------------------------------------ ;



; ==================================== ;
; Sec. 3: Identifiers and control-flow ;
; ==================================== ;

; # Identifiers (similar to constants in other languages, just an alias for a value)

; ## Global identifiers
(define PI 3.141592) ; accesible globaly in the program

; ## Local identifiers
(let ([cost 21] [budget 20]) (>= budget cost)) ; accesible only in the local scope

; # If statement
(if (>= 21 18) "adult" "child") ; order of arguments: condition, if-true, else

; ------------------------------------ ;



; ==================================== ;
;          Sec. 4: Functions           ;
; ==================================== ;

; # Definition
(define (min a b) (if (>= a b) b a)) ; order of arguments: define (fn_name arg1 arg2 ...) body
(min 2 3)                            ; function call

(define (factorial n)                ; example of a recursive function
  (if (= n 0)
      1
      (* n (factorial (- n 1)))
  )
)

(factorial 40)

; # Anonymous functions (a.k.a. lambda functions)
(lambda (n) (* n n)) ; order of arguments: lambda (arg1 arg2 ...) body
(λ (n m) (/ n m))    ; anonymous functions can also be defined with 'λ' (press CTRL+\ to type it in DrRacket)

; # Higher order functions (def.: function that takes other functions as arguments or that returns functions)

; ## Functions that return other functions
(define (mult_by_k k)         ; define the H.O. function
  (lambda (n) (* n k))
)
(define double (mult_by_k 2)) ; call H.O. function to define new function
(double 3)                    ; output: 6

; ## Functions that take other functions as arguments
(define (calc operation a b) (operation a b)) ; define H.O. function (the 'operation' arg is a function)
(calc (λ (a b) (/ (+ a b) 2)) 3 7)            ; call H.O. function (the 'operation' arg is a lambda that calculates the half-sum)

; ------------------------------------ ;



; ==================================== ;
;     Sec. 5: Filter, Fold & Map       ;
; ==================================== ;

; # Filter:
; Syntax: filter *condition* *list*
; Applies a filter to a list and returns a new list in order
; with the elements that satisfy the filter condition.
(filter even? (list 2 3 1 5 6 0)) ; output: '(2 6 0)

; Code for the class version:
(define (fn_filter condition lst)
  (if (empty? lst)
      empty
      (if (condition (car lst))
          (cons (car lst) (fn_filter condition (cdr lst)))
          (fn_filter condition (cdr lst))
      )
  )
)

; # Fold:
; Syntax: foldl/foldr *function* *default_val* *list*
; Applies a function to each element of the list (starting with a default value),
; accumulating the results in a single value, and returns said accumulated value.
; Note: it's worth pointing out that in each iteration, the accumulated value is
; always the second argument to the applied function, because of the definition of lists.
; This is: (*function* *element* *accumulator*) [in each iteration]

; ## foldl (applies function from LEFT to RIGHT)
(foldl / 1 (list 2 3 4 5)) ; would expand to: (/ 5 (/ 4 (/ 3 (/ 2 1) ) ) ) --> 15/8

; ## foldr (applies function from RIGHT to LEFT)
(foldr / 1 (list 2 3 4 5)) ; would expand to: (/ 2 (/ 3 (/ 4 (/ 5 1) ) ) ) --> 8/15

; Code for the class version (similar to foldr):
(define (fn_fold func initial-value lst)
  (if (empty? lst)
      initial-value
      (func (car lst) (fn_fold func initial-value (cdr lst)))
  )
)

; # Map
; Syntax: map *function* *list*
; Maps each element of a list to the result of applying a function to it
; in a new list, and returns said list.
(map sqrt (list 1 4 9))

; Code for the class version:
(define (fn_map function lst)
  (if (empty? lst)
      empty
      (cons (function (car lst)) (fn_map function (cdr lst)))
  )
)

; ------------------------------------ ;



; ==================================== ;
; Sec. 6: Parser, Interpreter & Runner ;
; ==================================== ;

; As we mentioned in Sec. 1, is possible to use the user-defined types
; to represent the grammar of a programming language. In this section we will
; apply this by creating a very small language, and the necesary elements for a
; program written on it to run, that is, a parser, an interpreter and a runner.

; # Grammar definition (a language that only supports numbers and sums)
(define-type Exp
  [num  (n number?)]
  [plus (left Exp?) (right Exp?)]
)

; # Parser (note: the input is the program string)
(define (parse s-expr)
  (match s-expr
    [(? number?) (num s-expr)]
    [(list '+ l r) (plus (parse l) (parse r))]
    [(list '++ e) (plus (parse e) (num 1))]    ; Added ++ as syntax sugar (this is: only the parser knows this syntax,
  )                                            ; and it translates it to proper language grammar, before passing it to
)                                              ; the interpreter)

; # Interpreter (note: the input is the parsed program string)
(define (interpret expr)
  (match expr
    [(num n) n]
    [(plus l r) (+ (interpret l) (interpret r))]
  )
)

; # Runner (note: the input is the program string, and the output is the result of running the program)
(define (run code)
  (interpret (parse code))
)

; # Testing the micro language
(test (run '1) 1)
(test (run '2.3) 2.3)
(test (run '{+ 1 2}) 3)
(test (run '{+ {+ 1 2} 3}) 6)
(test (run '{+ 1 {+ 2 3}}) 6)
(test (run '{+ 1 {+ {+ 2 3} 4}}) 10)
(test (run '{++ 1}) 2)
(test (run '{+ 1 {+ {+ 2 {+ 3 {++ 1}}} 4}}) 12)

; ------------------------------------ ;
