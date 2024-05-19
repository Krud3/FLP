#lang eopl
;; JUAN SEBASTIAN MOLINA CUELLAR 202224491-3743
;; CRISTIAN DAVID PACHECO TORRES 202227437-3743
;; PROYECTO FLP 2024-1
;; url GitHub: https://github.com/Krud3/FLP

;******************************
;;;;; Interpretador Taller 3

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <programa>          := <expresion>
;;                         un-program (exp)
;;  <expresion>         := <expresion> . <method>
;;  <expresion>         := <numero>
;;                         numero-lit (num)
;;
;;                      := "<texto>"
;;                         texto-lit (txt)

;;                      := <caracter>
;;                         caracter-lit (txt)

;;                      := <bool>
;;                         bool-lit (txt)
;;
;;                      := <identificador>
;;                         var-exp (id)
;;
;;                      := (<expresion> <primitiva-binaria> <expresion>)
;;                          primapp-bin-exp (exp1 prim-binaria exp2)

;;                      := "procedimiento" "(" <identificador-list> ")" "haga" <expression> "finProc"
;;                      := "evaluar" <expression> "(" <expression-list> ")" "finEval"
;;
;;                      := <primitiva-unaria> (<expresion>)
;;                          primapp-un-exp (prim-unaria exp)

;;                      := Si <expresion> entonces <expresion>  sino <expresion> finSI
;;                         condicional-exp (test-exp true-exp false-exp)

;;                      := procRec  {identifier ({identifier}(,)) = <expression>} in <expression>
;;                         <letrec-exp proc-names idss bodies bodyletrec>


;;                     := var {<identificador> = <expresion> }*(,) in <expresion>
;;                        var-mut-exp (exp)

;;                     := const { <identificador> = expresion> }*(,) in <expresion>
;;                        const-mut-exp (exp)

;;                     := rec { <identificador> = expresion> }*(,) in <expresion>
;;                        const-mut-exp (exp)

;;                     := <lista>
;;                        list-exp

;;                     := <vector>
;;                        vect-exp

;;                     := <registro>
;;                        reg-exp

;;                     := <exp-bool>
;;                        exp-bool


;;                     := begin {<expression>}+(;) end
;;                        block-exp (exp)

;;                     := if <exp-bool> then <expresion>
;;                        [ else <expression> ] end
;;                        if-exp (exp-bool true-block false-block)

;;                     := while <exp-bool> do
;;                        expresion done
;;                        while-exp (exp-bool exp) 

;;                     := for <identificador> = <expresion>
;;                        (to | down) <expression> do
;;                        <expresion> done
;;                        for-exp (exp1 exp2 block-exp)


;;  <lista>            := [{<expresion>}*(;) ]

;;  <vector>           := vector [{<expresion>}*(;) ]

;;  <registro>         := {{<identificador> = <expresion>}+(;)}

;;  <exp-bool>         := <pred-prim>(<expression, expression)>)
;;                     := <oper-bin-bool>(<exp-bool, exp-bool)>)
;;                     := <bool>
;;                     := <oper-un-bool>(<exp-bool>)

;;  <pred-prim>        := < | > | <= | >= | == | <>
;;                        log-pred-prim (exp1 exp2) 

;;  <oper-bin-bool>    := and | or
;;                        logical-exp (exp-bool exp-bool)

;;  <oper-un-bool>     := not
;;                        un-exp (exp-bool)

;;  <identificador>     := @<letter> | {<letter> | [0,...,9}*}  
;;                         var-exp (id)

;;  <letter>           := A..Z | a..z  
;;                        letter-exp (letter)
;;
;;  <primitiva-aritmetica> := +       (primitiva-suma)
;;                         := ~      (primitiva-resta)
;;                         := %      (primitiva-div)
;;                         := *      (primitiva-multi)

;; <primitiva-cadena>   :=  longitud (primitiva-longitud texto)
;;                      :=  concatenar (primitiva-concatenar texto texto)

;; <primitiva-lista>    := <empty-list> (empty-list)
;;                      := vacio?       (vacio?-exp list)
;;                      := crear-lista  (crear-lista-exp exprs)
;;                      := lista?       (lista?-exp expr)
;;                      := cabeza       (cabeza-list-exp list)
;;                      := cola         (cola-list-exp list)
;;                      := append       (append-list-exp list list) 


;; <primitiva-vector>   := vector?       (vector?-exp)
;;                      := crear-vector  (crear-vector-exp)
;;                      := ref-vector    (ref-vector-exp vector)
;;                      := set-vector    (lista?-exp vector exp)


;; <primitiva-registros>   := registro?       (registro?-exp)
;;                         := crear-registro  (crear-registro-exp)
;;                         := ref-registro    (ref-registro-exp registro)
;;                         := set-registro    (lista?-exp registro exp)

;; <empty-list>        := '() 

;******************************

;******************************

(define scanner-spec-interpreter
'((white-sp
   (whitespace) skip)
  (comment
   ("%" (arbno (not #\newline))) skip)
  (identificador
   ("@" letter (arbno (or letter digit "?"))) symbol)
  (vertice
   ( letter (arbno (or letter digit "?"))) symbol)
  (number
   (digit (arbno digit)) number)
  (number
   ("-" digit (arbno digit)) number)
  (number
   (digit (arbno digit)  "." digit (arbno digit)) number)
  (number
   ("-" digit (arbno digit) "." digit (arbno digit)) number)
  (texto
   (#\" (arbno (or letter digit " " "!" "?" ":" "." "," "-" "_" "/" "*" "&" "^" "%" "$" "#" "+" "=")) #\") string)
  (caracter
   (#\'  (or letter digit " " "!" "?" ":" "." "," "-" "_" "/" "*" "&" "^" "%" "$" "#" "+" "=") #\') string)
  (bool
   ((or "true" "false")) symbol)
  (call
   ("@" letter (arbno (or letter digit "?"))  ".") string)
  ))

;Especificación Sintáctica (gramática)

(define grammar-interpreter
  '((program (expression) un-programa)
    (expression (number) numero-lit)
    (expression (texto) texto-lit)
    (expression (caracter) caracter-lit)
    (expression (identificador) var-exp)
    
    (expression (primitiva-lista "(" (separated-list expression ",") ")")
                prim-list-exp)

    (expression (primitiva-vector "(" (separated-list expression ",") ")")
                prim-vec-exp)
    
    (expression
      ("(" expression primitiva-binaria expression ")")
      primapp-bin-exp
      )   
    (expression
      (primitiva-unaria "(" (separated-list expression ",") ")")
        primapp-un-exp)
<<<<<<< HEAD
=======
    (expression ("declarar" "(" (separated-list  expression ";") ")"
          "{" expression "}") 
            declarar-exp)
>>>>>>> 2c831c9e2d99ff80b1fa2100d164840da3590373
      (expression ("var" (separated-list identificador "=" expression ",") ";") variableLocal-exp)
      (expression ("const" (separated-list identificador "=" expression ",") ";") constLocal-exp)
      (expression ("procedimiento" "(" (separated-list identificador ",") ")" 
                                        "haga" expression "finProc")
                procedimiento-exp)
      (expression ("evaluar" expression "(" (separated-list expression ",") ")" "finEval")
                app-exp)
      (expression ("procRec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expression)  "in" expression) 
                procrec-exp)

      (expression (bool) bool-lit)
      (expression ("[" (separated-list expression ";") "]") lista-exp)
      (expression ("#" "[" (separated-list expression ";") "]") vector-exp)

      (expression ("{" identificador "=" expression 
                   (arbno ";" identificador "=" expression) "}") registro-exp)

      (expression
       (pred-prim "(" expression "," expression ")") predicate-exp)

      (expression
       (oper-bin-bool  expression "," expression ) bool-bin-exp)

      (expression
       (oper-un-bool "(" expression ")") bool-un-exp)

      (expression
       ("begin" expression 
                   (arbno ";" expression) "end") begin-exp)

      (expression
       ("for" identificador "=" expression "to" expression "do" expression "done") for-exp)

      (expression ("while" expression "do" expression "done") while-exp)
<<<<<<< HEAD

      (expression ("set" identificador "=" expression) set-exp)
      
      
      (expression ("print" "(" expression ")") print-exp)
      (expression ("Si" expression "entonces" expression "sino" expression "finSi") condicional-exp)
=======
      
      (expression ("print" "(" expression ")") print-exp)
      (expression ("if" expression "then" expression "end") if-condicional-exp)
      (expression ("ife" expression "then" expression "else" expression "end") elif-condicional-exp)

      (expression ("v" "(" (separated-list vertice ",") ")") vertice-dt-exp)
      (expression ("pv" "(" vertice "," vertice ")") pair-vertice-dt-exp)
      (expression ("edges" "(" (separated-list expression ",") ")") edges-dt-exp)
      (expression ("graph" "(" expression "," expression ")") graph-exp)

      ;(expression (call "vertices") obtener-vertices-exp)
      ;(expression (call "edges") obtener-edges-exp)

      (expression (call  llamado) obtener-call-exp)
      
      #|(expression
       (llamado) obtener-vertices-exp)|#

      (expression ("set" identificador "=" expression) set-exp)

      (llamado ("rest()") primitiva-rest)

      (llamado ("first()") primitiva-first)
      (llamado ("vertices") primitiva-llamado)
      (llamado ("edg") primitiva-edges)
      (llamado ("vecinos" "(" vertice ")") primitiva-vecinos)
      (llamado ("addEdge" "(" expression ")") primitiva-add-edge)
      ;(llamado (call "edg") primitiva-edg)
       
>>>>>>> 2c831c9e2d99ff80b1fa2100d164840da3590373
      (oper-un-bool      ("not")      primitiva-not)
      (oper-bin-bool     ("and")      primitiva-and)
      (oper-bin-bool     ("or")       primitiva-or)
      (pred-prim         ("<")        primitiva-menor)
      (pred-prim         (">")        primitiva-mayor)
      (pred-prim         ("<=")       primitiva-menor-igual)
      (pred-prim         (">=")       primitiva-mayor-igual)
      (pred-prim         ("==")       primitiva-igual)
      (pred-prim         ("!=")       primitiva-diferente)
      (primitiva-unaria  ("add1")     primitiva-add1)
      (primitiva-unaria  ("sub1")     primitiva-sub1)
      (primitiva-unaria  ("longitud") primitiva-longitud)
      (primitiva-binaria ("+")        primitiva-suma)
      (primitiva-binaria ("~")        primitiva-resta)
      (primitiva-binaria ("*")        primitiva-multi)
      (primitiva-binaria ("/")        primitiva-div)
      (primitiva-binaria ("concat")   primitiva-concat)

<<<<<<< HEAD
      (expression
          ("registro?" "(" identificador ")")
           primitiva-registro-is-registro
       )

      (expression
          ("crear-registro" "("  identificador "=" expression 
                   (arbno "," identificador "=" expression) ")")
           primitiva-registro-crear
      )

      (expression
          ("set-registro" "("  identificador ","
                   "{" (arbno  identificador "=" expression ";"  ) "}" ")")
           primitiva-registro-set
      ) 
      
      ;;(expression
        ;;  ("ref-registro" "(" identificador ")")
       ;;    primitiva-registro-ref
      ;;)
  
=======
      ;(graph-primitiva   (".")        )
      
      (primitiva-lista   ("vacio?")   primitiva-list-is-vacio)
      (primitiva-lista   ("vacio")    primitiva-list-vacio)
      (primitiva-lista   ("crear-lista")   primitiva-list-crear)
      (primitiva-lista   ("lista?")   primitiva-list-is-lista)
      (primitiva-lista   ("cabeza")   primitiva-list-cabeza)
      (primitiva-lista   ("cola")     primitiva-list-cola)
      (primitiva-lista   ("append")   primitiva-list-append)

      (primitiva-vector   ("vector?")      primitiva-vector-is-vector)
      (primitiva-vector   ("crear-vector") primitiva-vector-crear)
      (primitiva-vector   ("ref-vector")   primitiva-vector-ref)
      (primitiva-vector   ("set-vector")   primitiva-vector-set)
      
      (primitiva-registro   ("registros?")     primitiva-registro-is-registro)
      (primitiva-registro   ("crear-registro") primitiva-registro-crear)
      (primitiva-registro   ("ref-registro")   primitiva-registro-ref)
      (primitiva-registro   ("set-registro")   primitiva-registro-set) 
>>>>>>> 2c831c9e2d99ff80b1fa2100d164840da3590373
     )
    )



;;Construcion automatica de los data-types
(sllgen:make-define-datatypes scanner-spec-interpreter grammar-interpreter)


;;Para mostrar los tipos generados 
(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-interpreter grammar-interpreter)))


;*******************************************************************************************
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser scanner-spec-interpreter grammar-interpreter))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-interpreter grammar-interpreter))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop "--> "
    (lambda (pgm) (eval-program  pgm))
    (sllgen:make-stream-parser 
      scanner-spec-interpreter
      grammar-interpreter)))

;*****************************************************************************

(define-datatype tuple tuple?
  (pair (key symbol?)
        (value scheme-value?)
))

(define first-tuple
 (lambda (tp)
  (cases tuple tp
    (pair (first second) first)
    )
   )
  )

(define second-tuple
 (lambda (tp)
  (cases tuple tp
    (pair (first second) second)
    )
   )
  )

;; Define ele registro
(define-datatype register register?
  (register-record
        (records (list-of tuple?))
))

;Valido todo valor de Scheme
(define scheme-value?
  (lambda (v) #t)
)
(define cadena
  "hola")
;Definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
<<<<<<< HEAD
  (extended-env-record (ids (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (vars (list-of scheme-value?))
                       (env environment?)
=======
  (extended-env-record (ids            (list-of symbol?))
                       (vals            vector?)
                       (vars-mutablity (list-of boolean?))
                       (env             environment?)
>>>>>>> 2c831c9e2d99ff80b1fa2100d164840da3590373
  )
  (recursively-extended-env-record (proc-names (list-of symbol?))
                                   (idss (list-of (list-of symbol?)))
                                   (bodies (list-of expression?))
                                   (env environment?)))


;empty-env:      -> enviroment
;Función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))

;extend-env: <list-of symbols> <list-of numbers> <enviroment> -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (ids vals vars env)
<<<<<<< HEAD
    (extended-env-record ids vals vars env))) 
=======
    (extended-env-record ids (list->vector vals) vars env)))

>>>>>>> 2c831c9e2d99ff80b1fa2100d164840da3590373

;;Ambiente inicial v0
(define init-env
  (lambda ()
    (extend-env
     '(@a @b @c @d @e)
     '(1 2 3 "hola" "FLP")
     '(#t #f #t #t #t)
     (empty-env)
    )
  )
)


;; El punto de entrada del programa
(define eval-program
  (lambda (pgm) pgm
    (cases program pgm
      (un-programa (body) 
                     (eval-expression body (init-env))
      )
    )
  )
)


;eval-expression: <expression> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada
(define eval-expression
  (lambda (exp env)
   (cases expression exp
     (numero-lit (num) num)
<<<<<<< HEAD
     (print-exp (ex) (begin (display (eval-expression ex env)) 1))
=======
     (print-exp (ex) (begin (display (eval-expression ex env)) (newline)))
>>>>>>> 2c831c9e2d99ff80b1fa2100d164840da3590373
     (bool-un-exp (prim args) (not (eval-expression args env)))
     
     (vertice-dt-exp (ids)
        (ver-dt->list (eval-vertice-dt ids env)))
     
      (pair-vertice-dt-exp (left right)
        (pair-vertice-dt->list (pair-ver-dt left right)))
     
      (edges-dt-exp (pairs)
        (edges-dt->list (eval-lista-edges pairs env)))
     
     
     (graph-exp (vertices edges)
                (let ((graf (graph-dt ( ver-dt (eval-expression vertices env) ) 
                  (edg-dt (map (lambda (pair) (pair-ver-dt (car pair) (car (cdr pair)))) (eval-expression edges env))))
                            ))
                  (cases grafo graf
                    (graph-dt (vertices edges)
               (list 
               (cases vertice-dt vertices
                 (ver-dt (ver) ver)
                 (else '()))
               (cases edges-dt edges
                 (edg-dt (edges)
                   (map (lambda (pair)
                          (cases pair-vertice-dt pair
                            (pair-ver-dt (left right) (list left right))))
                        edges))
                 (else '())))))))

                     

     #|(graph-exp (vertices edges)
        (let ((v-list (eval-expression vertices env))
              (e-list (eval-expression edges env)))
          (graph-dt 
            (ver-dt v-list)
            (edg-dt (map (lambda (pair)
                           (pair-ver-dt (car pair) (cadr pair)))
                         e-list)))))|#
     ;(let ((symbol-id (remove-last-char id))))
     ;(car (apply-env env (string->symbol symbol-id)))
     #|(obtener-call-exp (call llamado)
         (let ((symbol-id (remove-last-char call)))           
        (apply-llamado llamado (string->symbol symbol-id) env)))|#
     (obtener-call-exp (call llamado)
        (let* ((symbol-id (remove-last-char call))
               (res (apply-llamado llamado (string->symbol symbol-id) env)))
          (if (procedure? res)
              (res (eval-expression (cadr llamado) env)) 
              res)))
     ;(obtener-vertices-exp (call)
     ;                      (let ((id (remove-last-char call)))
     ;                        (car (apply-env env (string->symbol id))))
                             ;(eval-expression (obtener-vertices (apply-env env (string->symbol id))) env))
     ;  )
     
     (prim-list-exp (prim rands)
                    (let ((args (eval-rands rands env)))
                        (apply-list-primitive prim args)
                      ))
     (prim-vec-exp (prim rands)
                    (let ((args (eval-rands rands env)))
                        (apply-vector-primitive prim args)
                      ))

     (predicate-exp (prim op1 op2)
                    (apply-pred-boolean prim (eval-expression op1 env)(eval-expression op2 env)))
     (bool-bin-exp (prim op1 op2)
                   (apply-bin-boolean prim (eval-expression op1 env)(eval-expression op2 env)))
<<<<<<< HEAD
     (variableLocal-exp (ids rands) 
                        (let ((args (eval-rands rands env)))
                            (extend-env ids args (make-filled-list #t (list-length ids)) env)))
   (begin-exp (exp exps)
              (let ((local-env
                     (cases expression exp
                       (variableLocal-exp (ids rands)
                                          (let ((args (eval-rands rands env)))
                                            (extend-env ids args (make-filled-list #t (list-length ids)) env)))
                       (constLocal-exp (ids rands)
                                       (let ((args (eval-rands rands env)))
                                         (extend-env ids args (make-filled-list #f (list-length ids)) env)))
                       (else env))))
                (let loop ((acc (eval-expression exp local-env))
                           (exps exps)
                           (acc-env local-env)) ;; Initialize acc-env with local-env
=======
     (declarar-exp (decls body)
        (let ((extended-env (eval-declarations decls env)))
          (eval-expression body extended-env)))
     #|(declarar-exp (id rands body) ids
                      (let ((args (eval-rands rands env)))
                            (eval-expression body (extend-env ids args env))))|#
     (variableLocal-exp (ids rands)
                        (begin (display ids)
                        (let ((args (eval-rands rands env)))
                            (extend-env ids args (make-filled-list #t (list-length ids)) env))))
   (begin-exp (exp exps)
              (let ((local-env
                     (begin
                     (cases expression exp
                       (variableLocal-exp (ids rands)                                           
                                           (let ((args (eval-rands rands env)))
                                            (extend-env ids args (make-filled-list #t (list-length ids)) env))
                                          )
                        (constLocal-exp (ids rands)
                                         (let ((args (eval-rands rands env)))
                                         (extend-env ids args (make-filled-list #f (list-length ids)) env))
                                     )
                       (else env)))))
                (let loop ((acc (eval-expression exp local-env))
                           (exps exps)
                           (acc-env env)) ;; Initialize acc-env with local-env
>>>>>>> 2c831c9e2d99ff80b1fa2100d164840da3590373
                  (if (null? exps)
                      acc ;; Return the accumulated result and env
                      (let ((result-env
                             (cases expression (car exps)
<<<<<<< HEAD
=======
                               ( app-exp (tor rands) local-env)
>>>>>>> 2c831c9e2d99ff80b1fa2100d164840da3590373
                               (variableLocal-exp (ids rands)
                                                  (let ((args (eval-rands rands acc-env)))
                                                    (extend-env ids args (make-filled-list #t (list-length ids)) acc-env)))
                               (constLocal-exp (ids rands)
                                               (let ((args (eval-rands rands env)))
                                                 (extend-env ids args (make-filled-list #f (list-length ids)) acc-env)))
                               (else acc-env))))
<<<<<<< HEAD
                        (loop (eval-expression (car exps) result-env)
                              (cdr exps)
                              result-env))))))

     (for-exp (ids init-value final-value body)
  (let loop ((counter (eval-expression init-value env)))
    (cond
      ((> counter (eval-expression final-value env)) '())  ; Base case: stop when counter reaches final value
      (else
       (eval-expression body env)    ; Evaluate the body
       (loop (+ counter 1))))))

     (while-exp (exp body)
=======
                        (begin #|(display "Before Init")
(display result-env)
(display "Before end")|#
                        (loop (cases expression (car exps)
(app-exp (tor rands) (eval-expression  (car exps) result-env))
                      (else
                       (begin
#|(display "Init")
(display result-env)
(display "end")|#
(eval-expression  (car exps) result-env))))
                               (cdr exps)
                              result-env)))))))

      (for-exp (id init-value final-value body)
               (let loop ((counter (eval-expression init-value env))
                          (env env))
                 (if (> counter (eval-expression final-value env))
                     'done
                     (let ((env (extend-env (list id) (list counter) (list #t) env)))
                       (eval-expression body env)
                       (loop (+ counter 1) env)))))

    (while-exp (exp body)
>>>>>>> 2c831c9e2d99ff80b1fa2100d164840da3590373
  (let loop ()
    (if (eval-expression exp env) ; Check condition
        (begin
          (eval-expression body env)     ; Execute body
          (loop))                        ; Recur if condition is true
<<<<<<< HEAD
        '())))                          ; Base case: condition is false


(set-exp (id rhs)
              (begin
                (let ((ref (apply-env-ref env id)))
                (cases reference ref
                  (a-ref (pos vec vars-mutability)
                         (if (list-ref vars-mutability pos)
                          (setref!
                            ref
                            (eval-expression rhs env))
                          (eopl:error  "Error, no se puede modificar una variable definida como constante: ~s" id)
                          )
                         )
                )
              )
                (newline)))
=======
        'done)))  

>>>>>>> 2c831c9e2d99ff80b1fa2100d164840da3590373
     
     (bool-lit (bool) (true-value? bool))
     (texto-lit (text) (trim-quotes text ))
     (caracter-lit (caracter) (trim-quote caracter))
     (var-exp (id) (apply-env env id))
<<<<<<< HEAD
     (vector-exp (elements) elements)
     (lista-exp (elements) elements)

     (registro-exp (key value rest-keys rest-values) (crear-registro-s key value rest-keys rest-values))
     (primitiva-registro-is-registro (id) (is-register (apply-env env id)))
     (primitiva-registro-crear (id rand rsstIds restRands) (crear-registro-s id rand rsstIds restRands)) 
     (primitiva-registro-set (id value key)  (eval-expression (set-exp (variableLocal-exp (list id) (set-register-field (register-to-list-pair (apply-env env id)) key value))) env))

     
=======
     (vector-exp (elements) (list->vector (eval-rands  elements env)))
     (lista-exp (elements) (eval-rands elements env))
     (registro-exp (key value key2 value2) (crear-registro key value key2 value2) )
>>>>>>> 2c831c9e2d99ff80b1fa2100d164840da3590373
     (primapp-bin-exp (lhs bin-op rhs) 
           (apply-binary-primitive bin-op (eval-expression lhs env) (eval-expression rhs env))
                      )
     (primapp-un-exp (un-op rands)
                      (let ((args (eval-rands rands env)))
                        (apply-unary-primitive un-op args)
                      )
                     )
     (elif-condicional-exp (test-exp true-exp false-exp) 
                      (if (eval-expression test-exp env)
                          (eval-expression true-exp env)
                          (eval-expression false-exp env)
                      )
     )
<<<<<<< HEAD
=======
     (if-condicional-exp (test-exp true-exp) 
                      (if (eval-expression test-exp env)
                          (eval-expression true-exp env)
                           #f
                      )
     )
     
     (set-exp (id rhs)
              (begin
                (let ((ref (apply-env-ref env id)))
                (cases reference ref
                  (a-ref (pos vec vars-mutability)
                         (if (list-ref vars-mutability pos)
                          (setref!
                            ref
                            (eval-expression rhs env))
                          (eopl:error  "Error, no se puede modificar una variable definida como constante: ~s" id)
                          )
                         )
                )
              )
                (newline)))
     
     
>>>>>>> 2c831c9e2d99ff80b1fa2100d164840da3590373
     (constLocal-exp (ids rands) ids
                      (let ((args (eval-rands rands env)))
                            (extend-env ids args (make-filled-list #f (list-length ids)) env)))
      (procedimiento-exp (ids body) (cerradura ids body env))

      (app-exp (rator rands)
               (let ((proc (eval-expression rator env))
                     (args (eval-rands rands env)))
                 (if (procVal? proc)
                     (apply-procedure proc args)
                     (eopl:error 'eval-expression
                                 "Attempt to apply non-procedure ~s" proc))))
      (procrec-exp (proc-names idss bodies letrec-body)
            (eval-expression letrec-body
                            (extend-env-recursively proc-names idss bodies env)))
      

     )))


(define (make-filled-list value n)
  (define (helper count acc)
    (if (= count 0)
        acc
        (helper (- count 1) (cons value acc))))
  (helper n '()))

(define (list-length lst)
  (if (null? lst)
      0
      (+ 1 (list-length (cdr lst)))))

<<<<<<< HEAD


(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env-ref "No binding for ~s" sym))
      (extended-env-record (syms vals vars-mutability old-env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vals vars-mutability)
                                 (apply-env-ref env sym))))
      
(recursively-extended-env-record (proc-names idss bodies old-env)
                                       (let ((pos (list-find-position sym proc-names)))
                                         (if (number? pos)
                                             (cerradura (list-ref idss pos)
                                                      (list-ref bodies pos)
                                                      env)
                                             (apply-env old-env sym))))
      )))


(define setref!
  (lambda (ref val)
    (primitive-setref! ref val)))

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec vars-mutability)
             (vector-set! vec pos val)))))


(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)
         (vars-mutability (list-of boolean?))
         ))

=======
>>>>>>> 2c831c9e2d99ff80b1fa2100d164840da3590373
;Función que aplica un ambiente a una variable y retorna el valor asociado a la variable
;Se utiliza para evaluar las variables en el ambiente
(define apply-env
  (lambda (env id)
    (cases environment env
      (empty-env-record () (eopl:error 'empty-env "Error, la variable no existe ~s" id))
<<<<<<< HEAD
      (extended-env-record (syms vals vars old-env)
=======
      (extended-env-record (syms vals vars-mutability old-env)
>>>>>>> 2c831c9e2d99ff80b1fa2100d164840da3590373
                           (let ((pos (list-find-position id syms)))
                             (if (number? pos)
                                 (vector-ref vals pos)
                                 (apply-env old-env id)
                             )
                           )
      )
      (recursively-extended-env-record (proc-names idss bodies old-env)
                                       (let ((pos (list-find-position id proc-names)))
                                         (if (number? pos)
                                             (cerradura (list-ref idss pos)
                                                      (list-ref bodies pos)
                                                      env)
                                             (apply-env old-env id)))))))

(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env-ref "No binding for ~s" sym))
      (extended-env-record (syms vals vars-mutability old-env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vals vars-mutability)
                                 (apply-env-ref env sym))))
      
(recursively-extended-env-record (proc-names idss bodies old-env)
                                       (let ((pos (list-find-position sym proc-names)))
                                         (if (number? pos)
                                             (cerradura (list-ref idss pos)
                                                      (list-ref bodies pos)
                                                      env)
                                             (apply-env old-env sym))))
      )))



(define setref!
  (lambda (ref val)
    (primitive-setref! ref val)))

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec vars-mutability)
             (vector-set! vec pos val)))))




                      
; funciones auxiliares para aplicar eval-expression a cada elemento de una 
; lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))




;Funcion auxiliar para evaluar un operando en un ambiente. Este es el llamado recursivo de eval-expression
(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))


;apply-primitive: <primitiva> <list-of-expression> -> numero
;Función que aplica una primitiva unaria a una lista de argumentos y retorna el resultado
;Se utiliza para evaluar las primitivas unarias
(define apply-unary-primitive
  (lambda (prim args)
    (cases primitiva-unaria prim
      (primitiva-add1 () (+ (car args) 1))
      (primitiva-sub1 () (- (car args) 1))
      (primitiva-longitud () (string-length (car args)))
      )))

(define apply-pred-boolean
  (lambda (prim op1 op2)
    (cases pred-prim prim
      (primitiva-menor () (< op1 op2))
      (primitiva-mayor () (> op1 op2))
      (primitiva-menor-igual () (<= op1 op2))
      (primitiva-mayor-igual () (>= op1 op2))
      (primitiva-igual () (equal? op1 op2))
      (primitiva-diferente () (not(equal? op1 op2)))
      )))

(define apply-bin-boolean
  (lambda (prim op1 op2)
    (cases oper-bin-bool prim
      (primitiva-and () (and op1 op2))
      (primitiva-or () (or op1 op2))
      )))
#|     (obtener-vertices-exp (call)
                           (let ((id (remove-last-char call)))
                             (car (apply-env env (string->symbol id))))
       )|#

(define apply-llamado
  (lambda (llam id env)
    (cases llamado llam
      (primitiva-llamado ()        
        (let ((value (lista-a-grafo (apply-env env id))))
          (if (grafo? value)
              (car (grafo->list value))
              (eopl:error "El valor no es un grafo" id))))
      (primitiva-edges ()                      
        (let ((value (lista-a-grafo (apply-env env id))))
          (if (grafo? value)
              (cdr (grafo->list value))
              (eopl:error "El valor no es un grafo" id))))
      (primitiva-vecinos (nodo) 
        (vecinos (lista-a-grafo (apply-env env id)) nodo))
      (primitiva-first () 
        (let ((value (apply-env env id)))
          (cond
            ((and (list? value) (not (null? value)) (symbol? (car value)))
             (let ((vertices (lista-a-vertice value)))
               (car (ver-dt->list vertices))))
            ((and (list? value) (not (null? value)) (pair? (car value)))
             (let ((edges (lista-a-edge value)))
               (car (edges-dt->list edges))))
            (else 
             (eopl:error "El valor no es una lista de vértices o ejes" id)))))
      (primitiva-rest () 
        (let ((value (apply-env env id)))
          (if (and (list? value) (not (null? value)) (pair? (car value)))
              (let ((edges (lista-a-edge value)))
                (cdr (edges-dt->list edges)))
              (eopl:error "El valor no es una lista de ejes" id))))

      (primitiva-add-edge (pair-ver-exp)
        (let* ((graph-value (apply-env env id))
               (graph (lista-a-grafo graph-value))
               (pair-ver (lista-a-pair-dt(eval-expression pair-ver-exp env))))
          
          (if (grafo? graph)
              (grafo->list (add-edge graph pair-ver))
              (eopl:error "El valor no es un grafo" id))))
      )))





#|(define apply-llamado
  (lambda (llam id env)
    (cases llamado llam
      (primitiva-llamado ()        
          (car (apply-env env id)))
      
      (primitiva-edges  ()                      
          (cdr (apply-env env id)))
      (primitiva-vecinos (nodo) 
          (vecinos (apply-env env id) nodo))
      )))|#
;(let ((symbol-id (remove-last-char id))))
;----------------------------------------------------------------------------------


(define apply-vector-primitive
  (lambda (prim args)
    (cases primitiva-vector prim
      (primitiva-vector-is-vector () (vector? (car args)))
      (primitiva-vector-crear () (apply vector args))
      (primitiva-vector-ref () (vector-ref (car args) (cadr args)))
      (primitiva-vector-set () (begin (vector-set! (car args) (cadr args) (caddr args)) (car args)))

      )))


(define apply-list-primitive
  (lambda (prim args)
    (cases primitiva-lista prim
      (primitiva-list-is-vacio () (null? args))
      (primitiva-list-vacio () '())
      (primitiva-list-crear () 
        (if (null? args)
            '()
            (cons (car args) (apply-list-primitive prim (cdr args)))))
      (primitiva-list-is-lista () (list? (car args)))
      (primitiva-list-cabeza () (car (car args)))
      (primitiva-list-cola () (cdr (car args)))
      (primitiva-list-append () (append args))
      )))

(define (apend lst)
  (define (flatten lst)
    (cond
      [(null? lst) '()]
      [(list? (car lst))
       (append (flatten (car lst)) (flatten (cdr lst)))]
      [else (cons (car lst) (flatten (cdr lst)))]))
  (flatten lst))
;valor-verdad? Determina si un valor dado corresponde a un valor booleano falso o verdadero
(define valor-verdad?
  (lambda (x)
   (not (zero? x))))

;apply-binary-primitive: <primitiva> <numero> <numero> -> <expression>
;Función que aplica una primitiva binaria a dos argumentos y retorna el resultado
;Se utiliza para evaluar las primitivas binarias
(define apply-binary-primitive
  (lambda (bin-prim arg1 arg2)
    (cases primitiva-binaria bin-prim
      (primitiva-suma   () (+ arg1 arg2))
      (primitiva-resta  () (- arg1 arg2))
      (primitiva-multi  () (* arg1 arg2))
      (primitiva-div    () (/ arg1 arg2))
      (primitiva-concat () (string-append arg1 arg2))
    )
  )
)


(define obtener-vertices
  (lambda (graph)
    (cases grafo graph
      (graph-dt (vertices edges)
        vertices))))

(define remove-last-char
  (lambda (s)
    (substring s 0 (- (string-length s) 1))))


(define true-value?
  (lambda (boolean-value)
     (eq? boolean-value 'true)
    )
)

(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)
         (vars-mutability (list-of boolean?))
         ))

(define-datatype tuple tuple?
  (pair (key symbol?)
        (value scheme-value?)
))

(define first-tuple
 (lambda (tp)
  (cases tuple tp
    (pair (first second) first)
    )
   )
  )

(define second-tuple
 (lambda (tp)
  (cases tuple tp
    (pair (first second) second)
    )
   )
  )


(define-datatype register register?
  (register-record
        (records (list-of tuple?))
))


(define crear-registro
  (lambda (key value key2 value2)
    ( let ((head (pair key value)  ))
     (register-record
      (cons head               
       (zip (lambda (k v)
           (pair k v))
         key2 value2 )
    ))))
  )


;***********************************************************************************************************************
;************************************************    Funciones Auxiliares    ̈*******************************************
;***********************************************************************************************************************


(define set-register-field
 (lambda (register key value)
(let loop ((reg register) (new-reg '()))
    (if (null? reg)
        (cons (cons key value) new-reg) ; Añadir el nuevo campo al final
        (let ((par (car reg)))
          (if (eq? (car par) key)
              ; Campo encontrado, actualizar el valor
              (append new-reg (cons (cons key value) (cdr reg)))
              ; Campo no encontrado, continuar con el siguiente
              (loop (cdr reg) (cons par new-reg))))))
   )
  )

(define register-to-list-pair
     (lambda (reg)
       (cases register reg
         (register-record (pairs)
           (map (lambda (p)
              (cases tuple p
                (pair (k v) (cons k v) )
              ))
            pairs))
 )))

(define (is-register value)
  (register? value)
)

(define crear-registro-s
  (lambda (key value key2 value2)
    ( let ((head (pair key value)  ))
     (register-record
      (cons head               
       (zip (lambda (k v)
           (pair k v))
         key2 value2 )
    )))
    )
  )

(define crear-registro
  (lambda (key value key2 value2)
    ( let ((head (pair key value)  ))
      (cons head               
       (zip (lambda (k v)
           (pair k v))
         key2 value2 )
    ))
    )
  )

(define zip 
  (lambda (f l1 l2)
    (cond
      [(or (null? l1) (null? l2)) '()]
      [(cons (f (car l1) (car l2)) (zip f (cdr l1) (cdr l2)))]
      )
  )
)

;trim-quotes: string -> string
;Función que elimina los caracteres dobles comillas agregadas cuando se parsea el string en el scaneo
(define (trim-quotes s)
  (let* ((first (string-ref s 0))
         (last (string-ref s (- (string-length s) 1)))
         (start (if (char=? first #\") 1 0))
         (end (if (char=? last #\") (- (string-length s) 1) (string-length s))))
    (substring s start end)))


(define (trim-quote s)
  (if (and (string? s) (> (string-length s) 0) (char=? (string-ref s 0) #\'))
      (substring s 1 (- (string-length s) 1))
      s))


; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de unambiente

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))
(define zip 
  (lambda (f l1 l2)
    (cond
      [(or (null? l1) (null? l2)) '()]
      [(cons (f (car l1) (car l2)) (zip f (cdr l1) (cdr l2)))]
      )
  )
)

(define eval-declarations
  (lambda (decls env)
    (if (null? decls)
        env
        (let ((decl (car decls))
              (extended-env (eval-declarations (cdr decls) env)))
          (cases expression decl
            (variableLocal-exp (ids rands)
                               (let ((args (eval-rands rands extended-env)))
                                 (extend-env ids args (make-filled-list #t (list-length ids)) extended-env)))
            (constLocal-exp (ids rands)
                            (let ((args (eval-rands rands extended-env)))
                              (extend-env ids args (make-filled-list #f (list-length ids)) extended-env)))
            
            (procedimiento-exp (ids body)
                               (let ((proc-name (car ids))
                                     (proc-args (cdr ids))
                                     (proc-body body))
                                 (extend-env (list proc-name)
                                             (list (cerradura proc-args proc-body extended-env))
                                             (list #f)
                                             extended-env)))
            (else (eopl:error 'eval-declarations "Unknown declaration type ~s" decl)))))))


;***********************************************************************************************************************
;*********************************************     Procedimientos     **************************************************
;***********************************************************************************************************************

(define-datatype procVal procVal?
  (cerradura
   (ids (list-of symbol?))
   (body expression?)
   (env environment?)))

;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args)
    (cases procVal proc
      (cerradura (ids body env)
<<<<<<< HEAD
               (eval-expression body (extend-env ids args (make-filled-list #f (list-length)) env))))))
=======
               (eval-expression body (extend-env ids args (make-filled-list #f (list-length ids)) env))))))
>>>>>>> 2c831c9e2d99ff80b1fa2100d164840da3590373

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (recursively-extended-env-record
     proc-names idss bodies old-env)))


;***********************************************************************************************************************
;*********************************************     graph        **************************************************
;***********************************************************************************************************************

;;------------------------------<vertices>--------------------------------

;;-------------------------constructor

;;<vertices-dt> ::= ()
;;              ::= (<Symbol> <vertices-dt>)

;; vertice-dt:
;; Proposito:
;; vertice-dt : Procedimiento encargado de definir el datatype de <vertices-dt>

(define-datatype vertice-dt vertice-dt?
  (empty-ver-dt)
  (ver-dt (vertices (list-of symbol?))))


;;----------------------------<pair-vertices>-----------------------------

;;-------------------------constructor

;;<pair-vertice-dt> ::= (<vertices-dt> <vertices-dt>)           

;; pair-vertice-dt:
;; Proposito:
;; pair-vertice-dt : Procedimiento encargado de definir el datatype de <pair-vertice-dt>

(define-datatype pair-vertice-dt pair-vertice-dt?
  (pair-ver-dt (left symbol?)
               (right symbol?)))

;;---------------------------------<edges>--------------------------------

;;-------------------------constructor

;;<edges-dt> ::= ()
;;           ::= (<pair-vertices-dt> <edges-dt>)

;; edges-dt:
;; Proposito:
;; edges-dt : Procedimiento encargado de definir el datatype de <edges>

(define-datatype edges-dt edges-dt?
  (empty-edge)
  (edg-dt (edges (list-of pair-vertice-dt?))))


;;---------------------------------<graph>--------------------------------

;;-------------------------constructor

;;<graph-dt> ::= (<vertices-dt> <edges-dt>)

;; grafo:
;; Proposito:
;; grafo : Procedimiento encargado de definir el datatype de <graph-dt>

(define-datatype grafo grafo?
  (graph-dt (v vertice-dt?)
            (ed edges-dt?)))


;-----------------------------------------------------------------------------------

(define eval-vertice-dt
  (lambda (ids env)
    (let loop ((ids ids) (acc '()))
      (if (null? ids)
          (ver-dt (reverse acc))
          (loop (cdr ids) (cons (car ids) acc))))))




(define eval-lista-edges
  (lambda (pairs env)
    (let loop ((pairs pairs) (acc '()))
      (if (null? pairs)
          (edg-dt (reverse acc))
          (let ((pair (eval-expression (car pairs) env)))
            (if (and (list? pair) (= (length pair) 2))
                (loop (cdr pairs) (cons (apply pair-ver-dt pair) acc))
                (eopl:error "Expected pair-vertice, found: " pair)))))))




(define ver-dt->list
  (lambda (v)
    (cases vertice-dt v
      (empty-ver-dt () '())
      (ver-dt (vertices) vertices))))

(define pair-vertice-dt->list
  (lambda (p)
    (cases pair-vertice-dt p
      (pair-ver-dt (left right) (list left right)))))

(define edges-dt->list
  (lambda (e)
    (cases edges-dt e
      (empty-edge () '())
      (edg-dt (edges)
        (map pair-vertice-dt->list edges)))))

(define grafo->list
  (lambda (g)
    (cases grafo g
      (graph-dt (vertices edges)
        (list (ver-dt->list vertices) (edges-dt->list edges))))))

;; vecinos:
;; Proposito:
;; vecinos : <graph-dt> <nodo> -> <list> : Funcion encargada de
;; encargada de dado un grafo y un nodo retornar una lista de los nodos
;; del grafo que tienen conexion directa con el nodo que entro por parametro

(define vecinos
  (lambda (graf nodo)
    (letrec ((buscar-vecinos
              (lambda (edg nodo)
                (cases edges-dt edg
                  (edg-dt (edges)
                          (let loop ((edges edges) (acc '()))
                            (if (null? edges)
                                acc
                                (let ((v (car edges))
                                      (next-vecinos (loop (cdr edges) acc)))
                                  (cases pair-vertice-dt v
                                    (pair-ver-dt (left right)
                                                 (cond
                                                   ((eqv? nodo left) (cons right next-vecinos))
                                                   ((eqv? nodo right) (cons left next-vecinos))
                                                   (else next-vecinos))))))))
                  (else '())))))
      (cases grafo graf
        (graph-dt (vertices edges)
                  (buscar-vecinos edges nodo))))))

(define lista-a-vertice
  (lambda (lst)
    (if (null? lst)
        (empty-ver-dt)
        (ver-dt lst))))

(define lista-a-pair-dt
  (lambda (lst)
    (if (or (null? lst) (not (= (length lst) 2)))
        (eopl:error "La lista debe tener exactamente dos elementos")
        (pair-ver-dt (car lst) (cadr lst)))))

(define lista-a-edge
  (lambda (lst)
    (if (null? lst)
        (empty-edge)
        (edg-dt (map (lambda (pair)
                       (if (and (list? pair) (= (length pair) 2))
                           (apply pair-ver-dt pair)
                           (eopl:error "Cada par en la lista debe tener exactamente dos elementos")))
                     lst)))))


(define lista-a-grafo
  (lambda (lst)
    (let ((vertices (car lst))
          (edges (cadr lst)))
      (graph-dt 
        (ver-dt vertices) 
        (edg-dt (map (lambda (pair) (apply pair-ver-dt pair)) edges))))))


(define first-element
  (lambda (value)
    (cond
      ((vertice-dt? value)
       (cases vertice-dt value
         (empty-ver-dt () (eopl:error "La lista de vértices está vacía"))
         (ver-dt (vertices) (if (null? vertices)
                                (eopl:error "La lista de vértices está vacía")
                                (car vertices)))))
      ((edges-dt? value)
       (cases edges-dt value
         (empty-edge () (eopl:error "La lista de ejes está vacía"))
         (edg-dt (edges) (if (null? edges)
                             (eopl:error "La lista de ejes está vacía")
                             (car edges)))))
      (else (eopl:error "El valor no es una lista de vértices o ejes")))))


;; ver-dt-verificator:
;; Proposito:
;; ver-dt-verificator : (<vertices-dt> <symbol>)-> bool:
;; Predicado encargado de validar si un value hace parte del conjunto
;; del entorno de un vertice

(define ver-dt-verificator
  (lambda (ver val)
    (cases vertice-dt ver
      (ver-dt (vertices)
              (if (member val vertices)
                  #t
                  #f))
      (else #f))))

;; pair-vertices-verificator:
;; Proposito:
;; pair-vertices-verificator : <pair-vertice-dt> <vertices-dt> -> bool :
;; Predicado encargado de validar si dos valores no son iguales
;; y si hacen parte del conjunto de vertices que llega por parametro.

(define pair-vertices-verificator
  (lambda (pair-ver ver)
    (cases pair-vertice-dt pair-ver
      (pair-ver-dt (left right)
                   (if (and (not (eqv? left right))
                            (and (ver-dt-verificator ver left)
                                 (ver-dt-verificator ver right)))
                       #t
                       #f)))))

;; edge-verificator:
;; Proposito:
;; edge-verificator : <edges-dt> <pair-vertice-dt> -> bool : Predicado encargado de
;; validar si un <pair-vertice-dt> ya esta contenido en el entorno de <edges-dt>

#|(define pair-in-edge?
  (lambda (edg pair-ver)
    (cases edges-dt edg
      (edg-dt (edges)
              (cases pair-vertice-dt pair-ver
                (pair-ver-dt (left right)
                             (if (or (member (list left right) (map pair-vertice-dt->list edges))
                                     (member (list right left) (map pair-vertice-dt->list edges)))
                                 #t
                                 (pair-in-edge? (edg-dt (cdr edges)) pair-ver)))))
      (else #f))))|#

(define pair-in-edge?
  (lambda (edg pair-ver)
    (cases edges-dt edg
      (empty-edge () #f)  ; Manejar el caso de empty-edge
      (edg-dt (edges)
              (if (null? edges)
                  #f
                  (cases pair-vertice-dt pair-ver
                    (pair-ver-dt (left right)
                                 (let ((current-edge (car edges)))
                                   (cases pair-vertice-dt current-edge
                                     (pair-ver-dt (v-left v-right)
                                                  (if (or (and (eq? left v-left) (eq? right v-right))
                                                          (and (eq? left v-right) (eq? right v-left)))
                                                      #t
                                                      (pair-in-edge? (edg-dt (cdr edges)) pair-ver))))))))))))



;; add-edge:
;; Proposito:
;; add-edge : <graph-dt> L <pair-vertice-dt> -> <graph-dt> L' : Funcion encargada de
;; adicionar el <pair-vertice-dt> a el grafo y retorna el grafo con su adicion
;; si no lo puede adicionar, retorna la estructura sin modificarla.
;; --------------------------------CUIDADO----------------------------------
;; TENER EN CUENTA QUE add-edge NO MODIFICA EL GRAFO SI NO QUE RETORNA UNO
;; NUEVO, si se quiere guardar el nuevo valor usar:
;; (define grafonuevo (add-edge grafoanterior pair-vertice))

(define add-edge
  (lambda (gra pair-ver)
    (cases grafo gra
      (graph-dt (vert edges)

        ;; Verificación y adición de la arista
        (if (and (not (pair-in-edge? edges pair-ver))
                 (pair-vertices-verificator pair-ver vert))
            ;; Descomponer edges en sus elementos de lista y concatenar con pair-ver
            (let ((edge-list (edges-dt->list edges)))
              (graph-dt vert (edg-dt (cons pair-ver (map (lambda (p) (pair-ver-dt (car p) (car(cdr p))) ) edge-list)))))
            gra)))))




