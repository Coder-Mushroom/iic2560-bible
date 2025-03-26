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
; program written in it to run, that is, a parser, an interpreter and a runner.

; # Grammar definition (a language that only supports numbers and sums)
(define-type NExp
  [n_num  (n number?)]
  [n_plus (left NExp?) (right NExp?)]
)

; # Parser (note: the input is the program string)
(define (n_parse s-expr)
  (match s-expr
    [(? number?) (n_num s-expr)]
    [(list '+ l r) (n_plus (n_parse l) (n_parse r))]
    [(list '++ e) (n_plus (n_parse e) (n_num 1))]    ; Added ++ as syntax sugar (this is: only the parser knows this syntax,
  )                                                  ; and it translates it to proper language grammar, before passing it to
)                                                    ; the interpreter)

; # Interpreter (note: the input is the parsed program string)
(define (n_interpret expr)
  (match expr
    [(n_num n) n]
    [(n_plus l r) (+ (n_interpret l) (n_interpret r))]
  )
)

; # Runner (note: the input is the program string, and the output is the result of running the program)
(define (n_run code)
  (n_interpret (n_parse code))
)

; # Testing the micro language
(test (n_run '1) 1)
(test (n_run '2.3) 2.3)
(test (n_run '{+ 1 2}) 3)
(test (n_run '{+ {+ 1 2} 3}) 6)
(test (n_run '{+ 1 {+ 2 3}}) 6)
(test (n_run '{+ 1 {+ {+ 2 3} 4}}) 10)
(test (n_run '{++ 1}) 2)
(test (n_run '{+ 1 {+ {+ 2 {+ 3 {++ 1}}} 4}}) 12)

; ------------------------------------ ;



; ==================================== ;
;     Sec. 7: Building a language      ;
; ==================================== ;

; In this section of the file, we will review how
; some of the elements and features that a programming
; language needs are implemented. For this, we will extend the
; small language defined in the previous section.
;
; Note: Because of how these features and elements will be implemented
; via Scheme, the notes will be structured a bit differently.

; # The Language Grammar
#|
Grammar for expressions:
<expr> ::= <num>
         | <id-symbol>                        ; For identifiers
         | {+ <expr> <expr>}
         | {- <expr> <expr>}
         | {if0 <expr> <expr> <expr>}
         | {with {<id-symbol> <expr>} <expr>} ; For identifiers
         | {<fn-id-symbol> <expr>}            ; For predefined functions
|#

; ===     (7.01) Identifiers       === ;

; Extension   : Support for local identifiers, similar to 'let' in Scheme.
; Syntax      : with {<id> <val-expr>} <body>
; Explanation : Using this syntax, the value assigned to <id> will be replaced
;               for the result of interpreting the <val-expr> in every corresponding
;               instance of it in <body>.

; In order to support local identifiers in the language, we will have to implement
; an algorithm to replace the identifiers with their values in the body of the
; 'with' block. This algorithm will replace eagerly, which means that all the
; instances will be swapped with the corresponding values before executing any
; code. Also, since we want these new identifiers to obey rules of scope
; (only existing in the body of the 'with' block that defined it), we have to
; be careful with when we replace, because a single identifier can be redefined
; in nested 'with' blocks. For example: { with {x 2} { with {x 3} x } } should
; return 3, but { with {x 2} { with {y 3} x } } should return 2.

; The algorithm described is called 'Substitution algorithm' and it's implemented
; later in the 'subs' function. Some things worth noting about it and it's implementation:

; • It replaces values in *expressions*, so it works with an alredy parsed program (in the AST).

; • In the grammar definition and parser, a 'with' case was added.

; • In the 'subs' function, in the 'id' case, the value is only substituted if the symbols are equal.

; • In the 'subs' function, in the 'with' case (which implies nested 'with' blocks), the value is always replaced
;   in the value expression of the nested 'with', but in the body only if the symbols are different
;   (this makes it so the scope rules are followed, and redefinitions behave as expected).

; • In the interpreter, a 'with' case was added, and it works by first interpreting the <val-exp> of the 'with' block (which
;   returns a number, so we wrap it in a 'num' so it can be placed in the AST), then applying the 'subs' function (with
;   the value found previously and the id) to the body of the 'with' block, and finally interpreting the resulting expression.

; === (7.02) Predefined Functions  === ;

; Extension   : Support for predefined (built-in) functions, written in the language.
; Syntax      : Calls: <fn-id> <arg-exp> | Def (in Scheme): (fundef '<fn-id> '<fn-arg> (parse '<fn-body>))
; Explanation : This feature makes it possible to write functions (using the language) as a predefined list,
;               and calling them in programs, similar to built-in functions in other languages.

; To support these predefined functions, we will have to add multiple things to the language, the ones to define
; the syntax for calls and the ones to be able to define the functions themselves.

; 1) To support function definition in the language backend:
; First, we add a new type to our grammar ('FunDef'), which has the function id, argument and body as fields.
; Then, since the functions are inside a predefined list that will be available for the interpreter, we need to
; be able to search a function definition given it's id, so the function 'lookup-fundef' is added to do just that.
; Also, to actually call and execute the functions, the function 'apply-fundef' is implemented, which works by
; first using the substitution algorithm to replace the argument id for it's value in the function body, and then
; interpreting the result of the substitution. Finally, we define 'pre-defined-funs', a list with all
; the definitions of our built-in functions.

; Note: Since the 'FunDef' type expects the body to be of type 'Exp' (an therefore, all the functions that work
; with 'FunDef's will as well), when a new function is defined, the body has to be parsed. For example, if we
; wanted to define a function that added 1 to a number n, we would write (fundef 'add1 'n (parse '{+ n 1}))
; insted of just (fundef 'add1 'n '{+ n 1}).

; 2) To support function calls:
; We start by adding a new case to our 'Exp' type ('call') with two arguments, the function id and the expresion for the
; argument. Then, we add the respective case to the parser, substitution function and interpreter. From these, the only
; one worth noting is the case for the interpreter: it works by calling 'apply-fundef' with the first argument being the
; return of calling 'lookup-fundef' (to see if the function actually exists), the second being the result of interpreting
; the argument expresion (wrapped in a 'num', since it will be used inside the call to 'subs' in 'apply-fundef'), and
; the last one being the list of predefined functions. It's also worth noting that, since the 'apply-fundef' function
; also calls 'interpret', a recursive predefined function or a predefined function that calls another one will work as
; expected.

; ==================================== ;

; # Grammar Definition

; ## General expresions
(define-type Exp
  [num  (n number?)]
  
  [plus (left Exp?) (right Exp?)]
  
  [minus (left Exp?) (right Exp?)]
  
  [if0 (econd Exp?) (etrue Exp?) (efalse Exp?)]
  
  [id  (s symbol?)]
  
  [with (id-symbol symbol?) (sexp Exp?) (bexp Exp?)]
  
  [call (fn-id-symbol symbol?) (aexp Exp?)]
)

; ## Type for predefined functions
(define-type FunDef
      [fundef  (fun-name symbol?) (arg-name symbol?) (body Exp?)]
)

; # Utility Functions

; ## Substitution algorithm
(define (subs sym value expr)
  (match expr
    [(num n) expr]
    
    [(id s) (if (symbol=? sym s)
                value
                expr)]
    
    [(plus l r) (plus (subs sym value l)
                      (subs sym value r)) ]
    
    [(minus l r) (minus (subs sym value l)
                        (subs sym value r)) ]
    
    [(if0 c t f) (if0 (subs sym value c)
                     (subs sym value t)
                     (subs sym value f))]
    
    [(with s se be) (if (symbol=? s sym)
                        (with s (subs sym value se) be)
                        (with s (subs sym value se) (subs sym value be)))]
    
    [(call fun-id ae) (call fun-id (subs sym value ae))]
  )
)

; ## For predefined functions support
(define (lookup-fundef f funs)
  (match funs
    ['() (error 'lookup-fundef "function not found: ~a" f)]
    
    [(cons (fundef fn _ _) rest)
     (if (symbol=? fn f)
         (car funs)
         (lookup-fundef f rest))]
  )
)

(define (apply-fundef fd arg-val funs)
  (match fd
    [(fundef _ arg-name fun-body)
     (interpret (subs arg-name arg-val fun-body) funs)]
  )
)

; # Parser
(define (parse s-expr)
  (match s-expr
    [(? number?) (num s-expr)]
    
    [(? symbol?) (id s-expr)]
    
    [(list '+ l r) (plus (parse l) (parse r))]
    
    [(list '- l r) (minus (parse l) (parse r))]
    
    [(list 'if0 c t f) (if0 (parse c) (parse t) (parse f))]

    [(list 'with (list sym se) be) (with sym (parse se) (parse be))]

    [(list fn-id-symbol ae) (call fn-id-symbol (parse ae))]
    
    [else (error 'parse "syntax error")]
  )
)

; # Interpreter
(define (interpret expr funs)
  (match expr
    [(num n) n]
    
    [(id s)  (error 'interpret "free identifier")]
    
    [(plus l r) (+ (interpret l funs) (interpret r funs))]
    
    [(minus l r) (- (interpret l funs) (interpret r funs))]
    
    [(if0 c t f) (if (eq? (interpret c funs) 0)
                     (interpret t funs)
                     (interpret f funs))]
    
    [(with s se be) (interpret (subs s (num (interpret se funs)) be) funs)]
    
    [(call fun-id ae) (apply-fundef (lookup-fundef fun-id funs)
                                    (num (interpret ae funs))
                                    funs)]
  )
)

; # Program Runner and Testing
(define (run funs prog)
  (interpret (parse prog) funs)
)

(define pre-defined-funs
  (list
   (fundef 'add1 'n (parse '{+ n 1}) )
   (fundef 'sum 'n (parse '{if0 n 0 {+ n {sum {- n 1}}}}) )
  )
)
 
; ## Tests

; ### For basic functionality
(test (run pre-defined-funs '2) 2)
(test (run pre-defined-funs '{+ 1 2}) 3)
(test (run pre-defined-funs '{- 1 2}) -1)
(test (run pre-defined-funs '{if0 {- 2 2} 0 1}) 0)
(test (run pre-defined-funs '{if0 {- 3 2} 0 1}) 1)

; ### For identifiers (7.01)
(test (run pre-defined-funs '{with {x 2} {+ x x}}) 4)                        ; Basic test
(test (run pre-defined-funs '{with {x 2} {with {y 3} {+ x y}}}) 5)           ; Nested non colliding withs 1
(test (run pre-defined-funs '{with {x 2} {with {y 2} {if0 {- x y} 5 7}}}) 5) ; Nested non colliding withs 2
(test (run pre-defined-funs '{with {x 2} {with {x 3} x}}) 3)                 ; Local scope (x redefined)
(test (run pre-defined-funs '{with {x 2} {with {y {+ x 2}} y}}) 4)           ; Sustitution for non colliding symbols (assingment)
(test (run pre-defined-funs '{with {x 2} {with {y {+ x 2}} x}}) 2)           ; Sustitution for non colliding symbols (body)

; ### For predefined functions (7.02)
(test (run pre-defined-funs '{+ {add1 2} 1}) 4)
(test (run pre-defined-funs '{sum 3}) 6)

; ------------------------------------ ;
