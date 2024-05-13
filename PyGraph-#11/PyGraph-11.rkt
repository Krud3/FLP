#lang eopl
;; JUAN SEBASTIAN MOLINA CUELLAR 202224491-3743
;; CRISTIAN DAVID PACHECO TORRES 202227437-3743
;; TALLER 3 FLP 2024-1
;; url GitHub: https://github.com/Krud3/FLP

;******************************
;;;;; Interpretador Taller 3

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <programa>          := <expresion>
;;                         un-program (exp)
;;
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
  ))

;Especificación Sintáctica (gramática)

(define grammar-interpreter
  '((program (expression) un-programa)
    (expression (number)  numero-lit)
    (expression (texto) texto-lit)
    (expression (caracter) caracter-lit)
    (expression (identificador) var-exp)
    (expression
      ("(" expression primitiva-binaria expression ")")
      primapp-bin-exp
      )   
    (expression
      (primitiva-unaria "(" (separated-list expression ",") ")")
        primapp-un-exp)
      (expression ("var" "(" (separated-list identificador "=" expression ";") ")"
          "{" expression "}") 
            variableLocal-exp)
      (expression ("const" "(" (separated-list identificador "=" expression ";") ")"
          "{" expression "}") 
            constLocal-exp)
      (expression ("procedimiento" "(" (separated-list identificador ",") ")" 
                                        "haga" expression "finProc")
                procedimiento-exp)
      (expression ("evaluar" expression "(" (separated-list expression ",") ")" "finEval")
                app-exp)
      (expression ("procRec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expression)  "in" expression) 
                procrec-exp)

      (expression (bool) bool-lit)
      (expression ("[" (separated-list expression ";") "]") lista-exp)
      (expression ("vector" "[" (separated-list expression ";") "]") vector-exp)

      (expression ("{" identificador "=" expression 
                   (arbno ";" identificador "=" expression) "}") registro-exp)
      
      (expression ("Si" expression "entonces" expression "sino" expression "finSi") condicional-exp)
      (primitiva-unaria  ("add1")     primitiva-add1)
      (primitiva-unaria  ("sub1")     primitiva-sub1)
      (primitiva-unaria  ("zero?")    primitiva-zero)
      (primitiva-unaria  ("longitud") primitiva-longitud)
      (primitiva-binaria ("+")        primitiva-suma)
      (primitiva-binaria ("~")        primitiva-resta)
      (primitiva-binaria ("*")        primitiva-multi)
      (primitiva-binaria ("/")        primitiva-div)
      (primitiva-binaria ("concat")   primitiva-concat)

      
  
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

;Valido todo valor de Scheme
(define scheme-value?
  (lambda (v) #t)
)

;Definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (ids (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (env environment?)
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
  (lambda (ids vals env)
    (extended-env-record ids vals env))) 

;;Ambiente inicial v0
(define init-env
  (lambda ()
    (extend-env
     '(@a @b @c @d @e)
     '(1 2 3 "hola" "FLP")
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
     (bool-lit (bool) (true-value? bool))
     (texto-lit (text) (trim-quotes text ))
     (caracter-lit (caracter) (trim-quote caracter))
     (var-exp (id) (apply-env env id))
     (vector-exp (elements) elements)
     (lista-exp (elements) elements)
     (registro-exp (key value key2 value2) key value key2 key2)
     (primapp-bin-exp (lhs bin-op rhs) 
           (apply-binary-primitive bin-op (eval-expression lhs env) (eval-expression rhs env))
                      )
     (primapp-un-exp (un-op rands)
                      (let ((args (eval-rands rands env)))
                        (apply-unary-primitive un-op args)
                      )
                     )
     (condicional-exp (test-exp true-exp false-exp) 
                      (if (eval-expression test-exp env)
                          (eval-expression true-exp env)
                          (eval-expression false-exp env)
                      )
     )
     (constLocal-exp (ids rands body) ids
                      (let ((args (eval-rands rands env)))
                            (eval-expression body (extend-env ids args env))))
     (variableLocal-exp (ids rands body) ids
                      (let ((args (eval-rands rands env)))
                            (eval-expression body (extend-env ids args env))))
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

;Función que aplica un ambiente a una variable y retorna el valor asociado a la variable
;Se utiliza para evaluar las variables en el ambiente
(define apply-env
  (lambda (env id)
    (cases environment env
      (empty-env-record () (eopl:error 'empty-env "Error, la variable no existe ~s" id))
      (extended-env-record (syms vals old-env)
                           (let ((pos (list-find-position id syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
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
      (primitiva-zero () (valor-verdad? (car args)))
      )))

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


(define true-value?
  (lambda (boolean-value)
     (eq? boolean-value 'true)
    )
)

;***********************************************************************************************************************
;************************************************    Funciones Auxiliares    ̈*******************************************
;***********************************************************************************************************************

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
               (eval-expression body (extend-env ids args env))))))

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (recursively-extended-env-record
     proc-names idss bodies old-env)))