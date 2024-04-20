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
   (letter (arbno (or letter digit "?"))) symbol)
   )
  )


;Especificación Sintáctica (gramática)

(define grammar-interpreter
  '((program (expression) un-programa)
    (expression (number) numero-lit)
    (expression ("\"" texto "\"") texto-lit)
    (expression (identificador) var-exp)
    (expression
      ("(" expression primitiva-binaria expression ")")
      primapp-bin-exp
      )    
    (expression
      (primitiva-unaria "(" (separated-list expression ",") ")")
        primapp-un-exp)
      (primitiva-unaria  ("add1")     primitiva-add1)
      (primitiva-unaria  ("sub1")     primitiva-sub1)
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

;(define interpretador
;  (sllgen:make-rep-loop "--> "
;    (lambda (pgm) (eval-program  pgm))
;    (sllgen:make-stream-parser 
;      scanner-spec-interpreter
;      grammar-interpreter)))