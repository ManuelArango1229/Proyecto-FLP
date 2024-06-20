#lang eopl

#| eopl.ss:  A simple language for the book "Essentials of Programming Languages"

Created by: Juan Manuel Arango, Juan Camilo Valencia,Edgar Andres Vargas, Juan Pablo Escobar 

Start: 2024-04-28

fundamentals of programming lenguages course project (FLP)|#


;;Define the lexical specification for the language
(define lexical-specification
  '(
    (white-sp (whitespace) skip)
    (comment ("//" (arbno (not #\newline))) skip)
    (identificador (letter (arbno (or letter digit "?"))) symbol)
    (digitoBinario ("b" (or "0" "1") (arbno (or "0" "1"))) string)
    (digitoBinario ("-" "b" (or "0" "1") (arbno (or "0" "1"))) string)
    (digitoDecimal (digit (arbno digit)) number)
    (digitoDecimal ("-" digit (arbno digit)) number)
    (digitoOctal ("0x" (or "0" "1" "2" "3" "4" "5" "6" "7")(arbno (or "0" "1" "2" "3" "4" "5" "6" "7"))) string)
    (digitoOctal ("-" "0x" (or "0" "1" "2" "3" "4" "5" "6" "7") (arbno (or "0" "1" "2" "3" "4" "5" "6" "7"))) string)
    (digitoHexadecimal ("hx" (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F") (arbno (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"))) string)
    (digitoHexadecimal ("-" "hx" (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F") (arbno (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"))) string) 
    (flotante (digit (arbno digit) "." digit (arbno digit)) number)
    (flotante ("-" digit (arbno digit) "." digit (arbno digit)) number)
  )
)

;; Define the grammar specification for the Language

(define grammar-specification
  '(
    (programa ( expresion) a-programa)
    (expresion (bool-expresion) bool-exp)
    (expresion (identificador) var-exp)
    (expresion (numero-exp) num-exp)
    (expresion ("\"" identificador (arbno identificador) "\"") cadena-exp)
    (expresion (var-decl) decl-exp)

    ;;Listas y arrays
    (expresion ("list" "(" (separated-list expresion ",") ")") lista-exp)
    (expresion ("cons" "(" expresion expresion ")") cons-exp)
    (expresion ("empty") empty-list-exp)
    (expresion ("array" "(" (separated-list expresion ",") ")") array-exp)

    ;;Expresion primitivas
    ;;Primitiva numerica
    (expresion ("(" expresion primitiva expresion ")") prim-num-exp)
    ;;Primitiva booleana
    (expresion (primitivaBooleana "(" (separated-list expresion ",") ")") prim-bool-exp)
    ;;Primitiva listas
    (expresion (primitivaListas "(" expresion ")") prim-list-exp)
    ;;Primitiva array
    (expresion (primitivaArray "(" (separated-list expresion ",") ")") prim-array-exp)
    ;;Primitiva de cadenas
    (expresion (primitivaCadena "(" (separated-list expresion ",") ")") prim-cad-exp)


    ;;Condicionales
    (expresion ("if" expresion "{" expresion "else" expresion "}") if-exp)


    ;;Iteradores
    (expresion ("for" identificador "from" expresion "until" expresion "by" expresion "do" expresion) for-exp)
    (expresion ("while" expresion "{" expresion "}") while-exp)

    ;;Switch
    (expresion ("switch" "(" expresion ")" "{" (arbno "case" expresion ":" expresion) "default" ":" expresion "}") switch-exp)

    ;;Secuenciación y asignación
    (expresion ("begin" expresion (arbno ";" expresion) "end") begin-exp)
    (expresion ("set" identificador "=" expresion) set-exp)

    ;;Funciones
    (expresion ("func" "(" (separated-list identificador ",") ")" expresion) func-exp)
    (expresion ("call" expresion "(" (separated-list expresion ",") ")") call-exp)

    ;;Instanciación y uso de estructuras
    (expresion ("new" identificador "(" (separated-list expresion ",") ")") new-struct-exp)
    (expresion ("get" expresion "." identificador) get-struct-exp)
    (expresion ("set-struct" expresion "." identificador "=" expresion) set-struct-exp)

    ;;Reconocimiento de patrones
    (expresion ("match" expresion "{" (arbno regular-exp "=>" expresion) "}") match-exp)

    ;;Numero-exp
    (numero-exp (digitoDecimal) decimal-num)
    (numero-exp (digitoOctal) octal-num)
    (numero-exp (digitoBinario) bin-num)
    (numero-exp (digitoHexadecimal) hex-num)
    (numero-exp (flotante) float-num)
    
    ;;Bool-exp
    (bool-expresion ("true") true-exp)
    (bool-expresion ("false") false-exp)

    ;;primitivas numéricas
    (primitiva ("+") sum-prim)
    (primitiva ("-") minus-prim)
    (primitiva ("*") mult-prim)
    (primitiva ("mod") mod-prim)
    (primitiva ("pow") elevar-prim)
    (primitiva ("<") menor-prim)
    (primitiva (">") mayor-prim)
    (primitiva ("<=") menorigual-prim)
    (primitiva (">=") mayorigual-prim)
    (primitiva ("!=") diferente-prim)
    (primitiva ("==") igual-prim)

    ;;primitiva booleana
    (primitivaBooleana ("and") and-prim)
    (primitivaBooleana ("or") or-prim)
    (primitivaBooleana ("xor") xor-prim)
    (primitivaBooleana ("not") not-prim)

    ;;Primitiva listas
    (primitivaListas ("first") first-primList)
    (primitivaListas ("rest") rest-primList)
    (primitivaListas ("empty?") empty-primList)

    ;;Primitiva arrays
    (primitivaArray ("length") length-primArr)
    (primitivaArray ("index") index-primArr)
    (primitivaArray ("slice") slice-primArr)
    (primitivaArray ("setlist") setlist-primArr)

    ;;Primitiva cadenas
    (primitivaCadena ("concat") concat-primCad)
    (primitivaCadena ("string-length") length-primCad)
    (primitivaCadena ("elementAt") index-primCad)
    
    ;;Variables
    (var-decl ("var" (arbno identificador "=" expresion) "in" expresion) lvar-exp)
    (var-decl ("let" (arbno identificador "=" expresion) "in" expresion) let-exp)
    
    ;;Estructuras de datos
    (struct-decl ("struct" identificador "{" (arbno identificador) "}") struct-exp)

    ;;Expresiones regulares
    (regular-exp (identificador "::" identificador) list-match-exp)
    (regular-exp ("numero" "(" identificador ")") num-match-exp)
    (regular-exp ("cadena" "(" identificador ")") cad-match-exp)
    (regular-exp ("boolean" "(" identificador ")") bool-match-exp)
    (regular-exp ("array" "(" (separated-list identificador ",") ")") array-match-exp)
    (regular-exp ("empty") empty-match-exp)
    (regular-exp ("default") default-match-exp)
    )
)

;; Create datatype automatically
(sllgen:make-define-datatypes lexical-specification grammar-specification)


;;Job function (evaluate the expression)
(define eval-exp
  (lambda (e env)
    (cases expresion e
      (num-exp (n) (eval-num n))
      (var-exp (id) (apply-env env id))
      ;;Primitive
      (prim-num-exp (exp1 prim exp2)
        (let
          (
            (Eexp1 (eval-exp exp1 env))
            (Eexp2 (eval-exp exp2 env))
          )
          (eval-prim Eexp1 prim Eexp2)
        )
      )
      ;;Booolean
      (bool-exp (bool) #T)
      ;;Conditional
      (if-exp (cond exp1 exp2)
        (if (eval-exp cond env) (eval-exp exp1 env) (eval-exp exp2 env))
      )
      ;Var 
      (decl-exp (var-dcl)
         (cases var-decl var-dcl
           (let-exp (ids rands body)
                    (let
                         (
                          (lvalues (map (lambda (x) (direct-target (eval-exp x env))) rands))
                          )
                       (eval-exp body (extend-env-let ids lvalues env))
                       )
                    
                    )
           (lvar-exp (ids rands body)
                     (let
                         (
                          (lvalues (map (lambda (x) (direct-target (eval-exp x env))) rands))
                          )
                       (eval-exp body (extend-env ids lvalues env))
                       )
                     )
           ) 
        
      )
      ;;Procedure
      (func-exp (ids body)
        (Closure ids body env)
      )
      (call-exp (rator rands)
        (let
          (
            (procV (eval-exp rator env))
            (lrands (map (lambda (x) (eval-rand x env)) rands))
          )
          (if (procval? procV) 
              (cases procval procV
                (Closure (lids body old-env)
                  (eval-exp body (extend-env lids lrands old-env))
                )
              )
            (eopl:error "Not a procedure " procV)
          )
        )
      )
      ;;begin
      (begin-exp (exp lexp)
        (if (null? lexp)
          (eval-exp exp env)
          (begin
            (eval-exp exp env)
            (letrec
              (
                (aux (lambda (lexp)
                  (cond
                    [(null? (cdr lexp)) (eval-exp (car lexp) env)]
                    [else (begin (eval-exp (car lexp) env) (aux (cdr lexp)))]
                  )
                  )
                )
              )
              (aux lexp)
            )
          )
        )
      )
      ;;Set
      (set-exp (id exp)
        (begin
            (setref! (apply-env-ref env id) (eval-exp exp env))
            1
        )
      )
      (for-exp (id from until by body)
               (let
                   (
                    (environment (extend-env-let (list id)  (list (direct-target (eval-exp from env))) env))
                    )
                 
                 (eval-for environment id from until by body)
)
               )
      (while-exp (condi body)
                (eval-while env condi body)
                 )
      (switch-exp (exp Lcases Lvalues default)
                  (let
                      (
                      (Eexp (eval-exp exp env))
                      (cases (map (lambda(x) (eval-exp x env)) Lcases))
                      (values (map (lambda(x) (eval-exp x env)) Lvalues))
                      (Edefault (eval-exp default env))
                      )
                    (eval-switch Eexp cases values Edefault)
                      )
                  )
      (cadena-exp (identifier Lidentifier) (slist->string (cons identifier Lidentifier)))
      (prim-cad-exp (prim Largs)
                    (let
                        (
                         (arg (eval-exp (car Largs) env))
                         (args (map (lambda (x) (eval-exp x env)) (cdr Largs)))
                         )
                      (eval-prim-cad prim arg args)
                        )
                    )
      (prim-bool-exp (prim Larg)
                       (eval-prim-bool prim Larg env)
                     )
      (lista-exp (args) (map (lambda (x)(eval-exp x env))args))
      (cons-exp (exp1 exp2) (cons (eval-exp exp1 env) (eval-exp exp2 env)))
      (empty-list-exp () '())
      (array-exp (args) (list->vector (map (lambda (x)(eval-exp x env))args)))
      (prim-list-exp (prim arg) (eval-prim-exp prim (eval-exp arg env)))
      (prim-array-exp (prim args) (eval-prim-array prim (map(lambda (x)(eval-exp x env)) args)))
      (else 1)
    )
  )
)

(define slice-vector
  (lambda (lst start end)
    (list->vector
   (let loop ((i start) (result '()))
     (if (< i end)
         (loop (+ i 1) (cons (vector-ref lst i) result))
         (reverse result)))))
    )
  
(define eval-prim-array
  (lambda (prim arg)
    (cases primitivaArray prim
    (length-primArr () (vector-length (car arg)))
    (index-primArr () (vector-ref (car arg) (car (cdr arg))))
    (slice-primArr () (slice-vector (car arg) (car (cdr arg)) (+ 1(car (cdr (cdr arg))))))
    (setlist-primArr () (vector-set! (car arg) (cadr arg) (caddr arg)))
    )
    )
  )
  
(define eval-prim-exp
  (lambda (prim arg)
    (cases primitivaListas prim
      (first-primList () (if (not (null? arg)) (car arg) (eopl:error "La lista esta vacia, no tiene first")))
      (rest-primList () (if (not (null? arg)) (cdr arg) (eopl:error "La lista esta vacia, no tiene rest")))
      (empty-primList () (null? arg))
      )
    )
  )
(define eval-prim-bool
  (lambda (prim args e)
    (cases primitivaBooleana prim
      (and-prim () (aux-and args e))
      (or-prim () (aux-or args e))
      (xor-prim () (aux-xor args e))
      (else 1)
      )
    )
  )
(define aux-and
  (lambda (args e)
    (letrec
        (
         (aux (lambda (x)
                (cond
                   [(null? x) #t]
                   [else
                    (and (eval-exp (car x) e) (aux (cdr x)))
                    ]
                   )
                   )
                )
              )
         
      (aux args)
    )
  )
  )
(define aux-or
  (lambda (args e)
    (letrec
        (
         (aux (lambda (x)
                (cond
                   [(null? x) #t]
                   [else
                    (or (eval-exp (car x) e) (aux (cdr x)))
                    ]
                   )
                   )
                )
              )
         
      (aux args)
    )
  )
  )

(define xor
  (lambda (a b)
  (if (and (not (and a b)) (or a b))
      #t
      #f)))
(define aux-xor
  (lambda (args e)
    (letrec
        (
         (aux (lambda (x)
                (cond
                   [(null? x) #t]
                   [else
                    (xor (eval-exp (car x) e) (aux (cdr x)))
                    ]
                   )
                   )
                )
              )
         
      (aux args)
    )
  )
  )
  
(define eval-prim-cad
  (lambda (prim arg args)
    (cases primitivaCadena prim
      (concat-primCad () (slist->string (map (lambda(x) (string->symbol x))(cons arg args))))
      (length-primCad () (string-length arg))
      (index-primCad () (string-ref arg (car args)))
      )
    )
  )
(define slist->string
  (lambda (slst)
  (cond ((null? slst) "")
        ((null? (cdr slst)) (symbol->string (car slst)))
        (else (string-append (symbol->string (car slst))
                             "" (slist->string (cdr slst)))))))
(define eval-switch
  (lambda (exp cases values default)
    (letrec
        (
         (aux (lambda (cases values)
              (cond
                [(null? cases) default]
                [(equal? exp (car cases)) (car values)]
                [else (aux (cdr cases) (cdr values))]
                )
              )
              )
         )
      (aux cases values)
        )
    )
  )

(define eval-while
  (lambda (e condi body)
    (letrec
        (
        (aux (lambda (e condi body)
             (cond
             
               [(not(eval-exp condi e)) '()]
               
               [else
                (cons (eval-exp body e) (aux e condi body))
                ])
               )
             )
             
        )
      (aux-for(aux e condi body))
        )
    )
  )



(define eval-for
  (lambda (e id from until by body)
    (let 
        (
         (Efrom (eval-exp from e))
         (Euntil (eval-exp until e))
         (Eby (eval-exp by e))
         )
    (letrec
        (
         (aux (lambda (env id from until by body)
              (cond
                [(> from (- until by)) '()]
                [else
                 (cons (eval-exp body env) (aux (extend-env-let (list id) (list (direct-target(+ from by))) env) id (+ from by) until by body))
                 ]
                )
              ))
         )
         (aux e id Efrom Euntil Eby body)
      )
    )
    )
  )

(define aux-for
  (lambda (x)
    (letrec
        (
         (aux (lambda (x)
                (cond
                  [(null? (cdr x)) (car x)]
                  [else (aux (cdr x))]
                  )
                ))
         )
      (aux x)
        )
    )
  )

(define eval-num
  (lambda (x)
    (cases numero-exp x
      (decimal-num (num) num)
      (octal-num (num) num)
      (bin-num (num) num)
      (hex-num (num) num)
      (float-num (num) num)
      )
    )
  )

;;Eval rand call-exp, exlude var-exp of direct-target

(define eval-rand
  (lambda (e env)
    (cases expresion e
      (var-exp (id)
        (indirect-target
          (let
            (
              (ref (apply-env-ref env id)) 
            )
            (cases target (primitive-deref ref)
              (direct-target (expval) ref)
              (indirect-target (ref2) ref2)
            )
          )
        )
      )
      (else
       (direct-target (eval-exp e env))
      )
    )
  )
)

;;Environment
(define-datatype env env?
  (empty-env)
  (extend-env-ref (Lid (list-of symbol?)) (LVal vector?) (env env?))
  (extend-env-let (Lid (list-of symbol?)) (Lval (list-of valor?)) (env env?))
)

(define valor?
  (lambda (x) #T)
  )

(define extend-env
  (lambda (ids vals env)
    (extend-env-ref ids (list->vector vals) env)
  )
)

;;Recursive environment

(define extend-recursive-env
  (lambda (procnames idss bodies old-env)
    (let
      (
       (vec-closure (make-vector (length procnames)))
      )
      (letrec
        (
          (amb (extend-env-ref procnames vec-closure old-env))
          (get-closure (lambda (ids bodies pos)
            (cond
              [(null? ids) amb]
              [else
                (begin
                  (vector-set! vec-closure pos (Closure (car ids) (car bodies) amb))
                  (get-closure (cdr ids) (cdr bodies) (+ pos 1))
                )
              ]
            )
            )
          )
        )
        (get-closure idss bodies 0)
      )
    )
  )
)

(define apply-env
  (lambda (e var)
    (let
        (
         (result (apply-env-ref e var))
         )
   (cond
     [(reference? result) (deref result)]
     [else (cases target result
             (direct-target (expval) expval)
             (else 1)
             )]
    )
      )
      )
  )
  


;;Apply env, function to apply the environment to the value
(define apply-env-ref
  (lambda (e id)
    (cases env e
      (empty-env () (eopl:error "Didn't find the value"))
      (extend-env-ref (lid vec old-env)
        (letrec
          (
            (aux (lambda (lid lval pos )
              (cond
                [(null? lid) (apply-env-ref old-env id)]
                [(eq? (car lid) id) (a-ref pos vec)]
                [else (aux (cdr lid) vec (+ pos 1))]
              )
              )
            )
          )
          (aux lid vec 0)
        ))
        (extend-env-let (lid lval old-env)
          (letrec
              (
            (aux (lambda (lid lval)
              (cond
                [(null? lid) (apply-env-ref old-env id)]   
                [(eq? (car lid) id) (car lval)]
                [else (aux (cdr lid) (cdr lval))]
              )

                        ))
            )
            (aux lid lval)
            ))
      )
    )
  )

(define eval-prim
  (lambda (arg1 prim arg2)
    (if
      (and (string? arg1) (string? arg2))
      (let
          (
           (tipo1 (substring arg1 0 2))
           (tipo2 (substring arg2 0 2))
           )
         (cond
        [(and (or (equal? tipo1 "-h") (equal? tipo1 "hx")) (or (equal? tipo2 "-h") (equal? tipo2 "hx")))
         (cond
          [(and (not (equal? (substring tipo1 0 1) "-")) (not (equal? (substring tipo2 0 1) "-")))
           (string-append "hx"
           (number->string (eval-prim-aux (string->number (substring arg1 2) 16) prim (string->number (substring arg2 2) 16)) 16)
           )
           ]
          [(and (equal? (substring tipo1 0 1) "-") (not (equal? (substring tipo2 0 1) "-")))
           (let
               (
           (result (number->string (eval-prim-aux (- (string->number (substring arg1 3) 16)) prim (string->number (substring arg2 2) 16)) 16))
           )
             (if (equal? (substring result 0 1) "-")
                 (string-append "-hx" (substring result 1))
                 (string-append "hx" result)
                 )
             )
           ]
          [(and (not (equal? (substring tipo1 0 1) "-")) (equal? (substring tipo2 0 1) "-"))
           (let
               (
           (result (number->string (eval-prim-aux (string->number (substring arg1 2) 16) prim (- (string->number (substring arg2 3) 16))) 16))
           )
             (if (equal? (substring result 0 1) "-")
                 (string-append "-hx" (substring result 1))
                 (string-append "hx" result)
                 )
             )
           ]
          [(and (equal? (substring tipo1 0 1) "-") (equal? (substring tipo2 0 1) "-"))
           (let
               (
           (result (number->string (eval-prim-aux (- (string->number (substring arg1 3) 16)) prim (- (string->number (substring arg2 3) 16))) 16))
           )
             (if (equal? (substring result 0 1) "-")
                 (string-append "-hx" (substring result 1))
                 (string-append "hx" result)
                 )
             )
           ]
          )
         ]
        )
      )
          
      
      (eval-prim-aux arg1 prim arg2)
      )
    )
  )




;;Function to evaluate the primitive

(define eval-prim-aux
  (lambda (arg1 prim arg2)
    (cases primitiva prim
      (sum-prim () (+ arg1 arg2))
      (minus-prim () (- arg1 arg2))
      (mult-prim () (* arg1 arg2))
      (mod-prim () (mod arg1 arg2))
      (elevar-prim () (expt arg1 arg2))
      (menor-prim () (< arg1 arg2))
      (mayor-prim () (> arg1 arg2))
      (igual-prim () (= arg1 arg2))
      (menorigual-prim () (<= arg1 arg2))
      (mayorigual-prim () (>= arg1 arg2))
      (diferente-prim () (not(equal? arg1 arg2)))
      )
    )
  )


(define (mod a b)
  (if (< a b)
      a
      (mod (- a b) b)))

;;Closure: Content information of procedure, linked to the place where it was created.
(define-datatype procval procval?
  (Closure
    (ids (list-of symbol?))
    (body expresion?)
    (environmet env?)
  )
)

;; Build the evaluator, this is a simple evaluator that just returns the value of the expression
(define eval-program
  (lambda (prog)
    (cases programa prog
      (a-programa (exp)(eval-exp exp init-env))
    )
  )
)
;; References

(define-datatype reference reference?
  (a-ref (pos number?) (vec vector?))
)

;;extract references
(define deref
  (lambda (ref)
     (cases target (primitive-deref ref)
      (direct-target (expval) expval)
      (indirect-target (ref1)
        (cases target (primitive-deref ref1)
          (direct-target (exp1) exp1)
          (indirect-target (p) (eopl:error 'derer "Illegal reference: ~s" ref1))
        )
      )
    )
  )
)


(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec) (vector-ref vec pos))
    )
  )
)
;;Asignation/Update references

(define setref!
  (lambda (ref val)
    (let
      (
        (ref1
          (cases target (primitive-deref ref)
            (direct-target (expval) ref)
            (indirect-target (ref2) ref2)
          )
        )
      )
      (primitive-setref! ref1 (direct-target val))
    )
  )
)

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec) (vector-set! vec pos val))
    )
  )
)

;; Step by reference

(define-datatype target target?
  (direct-target (expval expval?))
  (indirect-target (ref ref-to-direct-target?))
)


(define expval?
  (lambda (x)
    (or
      (number? x)
      (boolean? x)
      (procval? x)
      (string? x)
      (list? x)
      (vector? x)
    )
  )
)

(define ref-to-direct-target?
  (lambda (x)
    (and
      (reference? x)
      (cases reference x
        (a-ref (pos vec)
          (cases target (vector-ref vec pos)
            (direct-target (expval) #t)
            (indirect-target (ref) #f)
          )
        )
      )
    )
  )
)

;;Inittial environment
(define init-env
  (extend-env '(x y z) (list (direct-target 1) (direct-target 2) (direct-target 3)) (empty-env))
)
;; Build the stepper

  ;; Build the REPL
(define Interpreter
  (sllgen:make-rep-loop "-->" eval-program (sllgen:make-stream-parser lexical-specification grammar-specification))
)



(Interpreter)

