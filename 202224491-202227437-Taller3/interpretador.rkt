#lang eopl
;; JUAN SEBASTIAN MOLINA CUELLAR 202224491-3743
;; CRISTIAN DAVID PACHECO TORRES 202227437-3743
;; TALLER 3 FLP 2024-1

;******************************************************************************************
;;;;; Interpretador Taller 3

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <programa>          := <expresion>
;;                         un-program (exp)
;;
;;  <expresion>         := <numero>
;;                         numero-lit (num)
;;
;;                      := "\""<texto> "\""
;;                         texto-lit (txt)
;;
;;                      := <identificador>
;;                         var-exp (id)
;;
;;                      := (<expresion> <primitiva-binaria> <expresion>)
;;                          primapp-bin-exp (exp1 prim-binaria exp2)
;;
;;                      := <primitiva-unaria> (<expresion>)
;;                          primapp-un-exp (prim-unaria exp)

;;  <expresion>         := Si <expresion> entonces <expresion>  sino <expresion> finSI
;;                         condicional-exp (test-exp true-exp false-exp)
;;
;;  <primitiva-binaria> := +       (primitiva-suma)
;;                      :=  ~      (primitiva-resta)
;;                      :=  /      (primitiva-div)
;;                      :=  *      (primitiva-multi)
;;                      :=  concat (primitiva-concat)

;; <primitiva-unaria>   :=  longitud (primitiva-longitud)
;;                      :=  add1     (primitiva-add1)
;;                      :=  sub1     (primitiva-sub1)

;******************************************************************************************

;******************************************************************************************
;Especificación Léxica

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
   (digit (arbno digit) (or "." ",") digit (arbno digit)) number)
  (number
   ("-" digit (arbno digit) (or "." ",") digit (arbno digit)) number)
  (texto
   (#\" (arbno (or letter digit " " "!" "?" ";" ":" "." "," "-" "_" "/" "*" "&" "^" "%" "$" "#" "@" "+" "=")) #\") string)
  ))

;Especificación Sintáctica (gramática)

(define grammar-interpreter
  '((program (expression) un-programa)
    (expression (number)  numero-lit)
    (expression (texto) texto-lit)
    (expression (identificador) var-exp)
    (expression
      ("(" expression primitiva-binaria expression ")")
      primapp-bin-exp
      )   
    (expression
      (primitiva-unaria "(" (separated-list expression ",") ")")
        primapp-un-exp)
      (expression ("declarar" "(" (separated-list identificador "=" expression ";") ")"
          "{" expression "}") 
            variableLocal-exp)
      (expression ("procedimiento" "(" (separated-list identificador ",") ")" 
                                        "haga" expression "finProc")
                procedimiento-exp)

      (expression ("Si" expression "entonces" expression "sino" expression "finSI") condicional-exp)                   
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
  (extended-env-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (env environment?)
  )
)

;empty-env:      -> enviroment
;Función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))

;extend-env: <list-of symbols> <list-of numbers> <enviroment> -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env))) 

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
     (texto-lit (text) (trim-quotes text ))
     (var-exp (id) (apply-env env id))
     (primapp-bin-exp (lhs bin-op rhs) 
           (apply-binary-primitive bin-op (eval-expression lhs env) (eval-expression rhs env))
                      )
     (primapp-un-exp (un-op rands)
                      (let ((args (eval-rands rands env)))
                        (apply-unary-primitive un-op args)
                      )
                     )
     (condicional-exp (test-exp true-exp false-exp) 
                      (if (valor-verdad? (eval-expression test-exp env))
                          (eval-expression true-exp env)
                          (eval-expression false-exp env)
                      )
     )
     (variableLocal-exp (ids rands body) ids
                      (let ((args (eval-rands rands env)))
                            (eval-expression body (extend-env ids args env))))
      (procedimiento-exp (ids body) (cerradura ids body env))
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
    )
  )
)
                      
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


;***********************************************************************************************************************
;************************************************    Funciones Auxiliares    ̈*******************************************
;***********************************************************************************************************************

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