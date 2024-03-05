#lang eopl
;; JUAN SEBASTIAN MOLINA CUELLAR 202224491-3743
;; CRISTIAN DAVID PACHECO TORRES 202227437-3743
;; TALLER 1 FLP 2024-1


;; -----------------------1.1.1------------------------------ ;;
;; Ejercicio 1
;; invert:
;; Proposito:
;; invert : <List-pair> L -> <List-pair> L' : Procedimiento que retorna 
;; una lista similar a L con sus pares ordenados invertidos.
;;
;; <Pair> ::= (<Scheme-Value> <Scheme-Value>)
;;
;; <List-pair> ::= ()
;;             ::= (<Pair> <List-pair>)

(define invert
  (lambda (lst)
    (if (null? lst)
        empty
        (append (list (list (car (cdr (car lst))) (car (car lst))))
                (invert (cdr lst))
                )
        )
    )
  )
(invert '((a 1) (a 2) (1 b) (2 b)))
(invert '((5 9) (10 91) (82 7) (a e) ("hola" "Mundo")))
(invert '(("es" "racket") ("genial" "muy") (17 29) (81 o)))

;; Ejercicio 2
;; down:
;; Proposito:
;; down : <List> L -> <List> L':
;; Procedimiento que retorna una lista de cada elemento de L asociado a
;; un nivel mas de parentesis comparado con su estado original en L.
;;
;; <List> ::= ()
;;        ::= (<Scheme-Value> <List>)


;; Ejercicio 3
;; list-set:
;; Proposito:
;; list-set : <List> L <Int> n <Scheme-Value> x -> <List> L':
;; Procedimiento que teniendo en cuenta una lista L un numero "n" y
;; un elemento "x" retorna una lista similar L', pero en la posicion
;; "n" el elemento "x".
;; 
;; <List> ::= ()
;;        ::= (<Scheme-Value> <List>)

(define list-set
  (lambda (lst n x)
    (if (= n 0)
        (cons x  (cdr lst))
        (cons (car lst) (list-set (cdr lst) (- n 1) x))
        )
    )
  )
(list-set '(a b c d) 2 '(1 2))
(list-set '(a b c d) 3 '(1 5 10))
(list-set '(1 2 3 4 5) 2 '(a b))

;; Ejercicio 4
;; filter-in:
;; Proposito:
;; filter-in : <Predicado> P <List> L -> <List> L': Procedimiento
;; que retorna la lista que contiene los elementos de la lista L
;; que satisfacen el predicado P.
;;
;; <Predicado> ::= <Function-Name>
;;
;; <Function-Name> ::= <identificador> ; -> #t #f
;;
;; <List> ::= ()
;;        ::= (<Scheme-Value> <List>)


;; Ejercicio 5
;; mix:
;; Proposito:
;; mix : <List> L1 <List> L2 -> <List> L': Procedimiento que al 
;; recibir dos argumentos de tipo lista retorna una sola lista 
;; con los elementos cruzados entre ambas listas.
;;
;; <List> ::= ()
;;        ::= (<Scheme-Value> <List>)

(define mix
  (lambda (lst-1 lst-2)
    (if (null? lst-1)
        empty
        (append (list (car lst-1) (car lst-2))
                (mix (cdr lst-1) (cdr lst-2))
                )
        )
    )
  )
(mix '(1 2 3 4 5) '(6 7 8 9 10))
(mix '(a b c) '(7 8 9))
(mix '(1 (5 2) foo) '(h (1 3) flp))

;; Ejercicio 6
;; swapper:
;; Proposito:
;; swapper : <Scheme-Value> E1 <Scheme-Value> E2 <List> L -> <List> L':
;; Procedimiento que retorna una lista similar a L, solo que cada 
;; ocurrencia anterior de E1 sera remplazada por E2 y cada ocurrencia
;; E2 sera remplazada por E1 (E1 Y E2 pertenecen a L).
;;
;; <List> ::= ()
;;        ::= (<Scheme-Value> <List>)


;; Ejercicio 7
;; cartesian-product:
;; Proposito:
;; cartesian-product : <List-Symbol> L1 <List-Symbol> L2 -> <List-pair> L':
;; Procedimiento que retorna la lista de tuplas que representan el 
;; producto cartesiano entre L1 y L2.
;;
;; <List-Symbol> ::= ()
;;               ::= (<Symbol> <List-Symbol>)
;;
;; <Pair> ::= (<Scheme-Value> <Scheme-Value>)
;;
;; <List-pair> ::= ()
;;             ::= (<Pair> <List-pair>)

(define cartesian-product
  (lambda (lst-1 lst-2)
    (if (null? lst-1)
        empty
        (append (car-pro-helper lst-2 (car lst-1))
                (cartesian-product (cdr lst-1) lst-2)
                )
        )
    )
  )
(define car-pro-helper
  (lambda (lst val)
    (if (null? lst)
        empty
        (cons (list val (car lst)) (car-pro-helper (cdr lst) val))
        )
    )
  )
(cartesian-product '(a b c) '(x y))
(cartesian-product '(p q r) '(5 6 7))

;; Ejercicio 8
;; mapping:
;; Proposito:
;; mapping : <lambda-exp> F <List> L1 <List> L2 -> <List-pair> L':
;; Procedimiento que toma una función unaria F, y dos listas de números
;; L1 y L2 de igual tamaño.  Retorna una lista de pares L' donde cada par
;; (a, b) cumple que al aplicar F sobre a, el resultado es igual a b.
;; Solo se incluyen en L' aquellos pares donde se cumple esta condición.

;;
;; <lambda-exp> ::= <identificador>
;;              ::= (lambda (<identificador>) <lambda-exp>)
;;              ::= (<lambda-exp> <lambda-exp>)
;;
;; <List> ::= ()
;;        ::= (<Scheme-Value> <List>)
;;
;; <Pair> ::= (<Scheme-Value> <Scheme-Value>)
;;
;; <List-pair> ::= ()
;;             ::= (<Pair> <List-pair>)


;; Ejercicio 9
;; reverse:
;; Proposito:
;; reverse : <List> L -> <List> L': Procedimiento en cargado de retornar
;; la lista que se le entrega por parametro pero invertida.
;;
;; <List> ::= ()
;;        ::= (<Scheme-Value> <List>)

(define reverse
  (lambda (lst)
    (if (null? lst)
        empty
        (append (reverse (cdr lst))
                (list (car lst)))
        )
    )
  )
(reverse '(2 3 8 6 1))
(reverse '(1 2 3 4))
(reverse '(h o l a m u n d o))

;; Ejercicio 10
;; flatten:
;; Proposito:
;; flatten : <List> L -> <List> L': Procedimiento encargado de retornar
;; la lista "aplanada" que le entro como parametro, es decir sin ningun
;; tipo de lista anidada interna conservando todos los elementos.
;;
;; <List> ::= ()
;;        ::= (<Scheme-Value> <List>)


;; Ejercicio 11
;; unzip:
;; Proposito:
;; unzip : <List-pair> L -> (<List> <List>) L':
;; Dada una lista de tuplas (listas de dos elementos), retorna un par
;; de listas: la primera lista contiene todos los primeros elementos 
;; de las tuplas y la segunda lista todos los segundos elementos de
;; las tuplas.
;;
;; <List> ::= ()
;;        ::= (<Scheme-Value> <List>)
;;
;; <Pair> ::= (<Scheme-Value> <Scheme-Value>)
;;
;; <List-pair> ::= ()
;;             ::= (<Pair> <List-pair>)

(define unzip
  (lambda (lst)
    (list (car-unzip lst) (car-cdr-car-unzip lst))))

(define car-unzip
  (lambda (lst)
    (if (null? lst)
        empty
        (append (list (car (car lst)))
                (car-unzip (cdr lst)))
        )
    )
  )

(define car-cdr-car-unzip
  (lambda (lst)
    (if (null? lst)
        empty
        (append (list (car (cdr (car lst))))
                (car-cdr-car-unzip (cdr lst)))
        )
    )
  )
(unzip '((1 2) (3 4) (5 6)))
(unzip '((a 1) (b 2) (c 3)))

;; Ejercicio 12
;; scan:
;; Proposito:
;; scan : <List> L <Scheme-value> n <lambda-binary> F -> <List> L':
;; Procedimiento encargado de tomar el elemento n y aplicar la
;; funcion binaria F con cada elemento de la lista de forma acomulativa
;; retornando una lista con cada resultado parcial empezando con el
;; elemento n.
;;
;; <lambda-binary> ::= (lambda (<identificador> <identificador>) <expresion>)
;;
;; <List> ::= ()
;;        ::= (<Scheme-Value> <List>)


;; Ejercicio 13
;; operate:
;; Proposito:
;; operate :
;;         <List-lambda-binary> lrators <List-Int> lrands -> <Int> R:
;; Funcion encargada de retornar el resultado de aplicar sucesivamente
;; las operaciones en lrators a los valores en lrands.
;;
;; <lambda-binary> ::= (lambda (<identificador> <identificador>) <expresion>)
;;
;; <List-lambda-binary> ::= ()
;;                      ::= (<lambda-binary> <List-lambda-binary>)
;;
;; <List-Int> ::= ()
;;            ::= (<Int> <List-Int>)

(define operate
  (lambda (lrators lrands)
    (if (null? (cdr lrators))  
      ((car lrators) (car lrands) (cadr lrands))  
      (operate (cdr lrators)  
               (cons ((car lrators) (car lrands) (cadr lrands))  
                     (cddr lrands)
                     )
               )
      )
    )  
  )  
(operate (list + * + - *) '(1 2 8 4 11 6))
(operate (list *) '(4 5))

;; -----------------------1.1.2------------------------------ ;;
;; Ejercicio 14
;; path:
;; Proposito:
;; path : <Int> n <arbol-binario> A -> <route-List> L:
;; Funcion encargada de retornar la lista con la ruta a tomar,
;; indicando con cadenas left y right hasta llegar al numero n
;; recibido por parametro. Si el numero n es encontrado en el
;; nodo raiz, el procedimiento debe retornar una lista vacia.
;;
;; <arbol-binario> ::= ()
;;                 ::= (<Int> <arbol-binario> <arbol-binario>)
;;
;; <route-List> ::= ()
;;              ::= ('right <route-List>) | ('left <route-List>)


;; Ejercicio 15
;; inorder:
;; Proposito:
;; inorder : <arbol-binario> A -> <List> L: Encargada de tomar un
;; arbol binario y retornar una lista con los elementos del arbol
;; correspondiente al recorrelo inorder.
;;
;; <arbol-binario> ::= ()
;;                 ::= (<Int> <arbol-binario> <arbol-binario>)
;;
;; <List> ::= ()
;;        ::= (<Scheme-Value> <List>)

(define inorder
  (lambda (arbol)
    (if (null? arbol)
        empty
        (append (inorder (cadr arbol))
                (list (car arbol))
                (inorder (caddr arbol))
                )
        )
    )
  )
(inorder '(21 (0 (4 () ()) (4 () ()))(52 (14 (6 (8 () ()) ())(5 () ()))(19 () ()))))
(inorder '(14 (7 () (12 () ()))(26 (20 (17 () ())())(31 () ()))))

;; Ejercicio 16
;; Operar-binarias:
;; Proposito:
;; Operar-binarias : <OperacionB> O -> <Int> R: Funcion que recibe
;; como parametro una operacion binaria valida y retorna el resultado
;; de hacer las operaciones suma, resta y multiplicacion correspondientes.
;;
;; <OperacionB> ::= <Int>
;;              ::= (<OperacionB> 'suma <OperacionB>)
;;              ::= (<OperacionB> 'resta <OperacionB>)
;;              ::= (<OperacionB> 'multiplica <OperacionB>)


;; Ejercicio 17
;; prod-scalar-matriz:
;; Proposito:
;; prod-scalar-matriz : <matrz> mat <vect> vec -> <List> R: Procedimiento
;; encargado de recibir una matriz mat y un vector vec para retornar el
;; resultado de realizar la multipliacion matriz por vector.
;;
;; <row> ::= ()
;;       ::= (<Int> <row>)
;;
;; <matrz> ::= ()
;;         ::= (<row> <matrz>)
;;
;; <vect> ::= ()
;;        ::= (<Int> <vect>)
;;
;; <List> ::= ()
;;        ::= (<Scheme-Value> <List>)

(define prod-scalar-matriz
  (lambda (mat vec)
    (if (null? mat)
        empty
        (cons (p-s-m-helper (car mat) vec)
                (prod-scalar-matriz (cdr mat) vec)
                )
        )
    )
  )

(define p-s-m-helper
  (lambda (lst-1 lst-2)
    (if (null? lst-1)
        empty
        (cons (* (car lst-1)(car lst-2))
              (p-s-m-helper (cdr lst-1) (cdr lst-2))
              )
        )
    )
  )
(prod-scalar-matriz '((1 1) (2 2)) '(2 3))
(prod-scalar-matriz '((1 1) (2 2) (3 3)) '(2 3))


;; Ejercicio 18
;; pascal:
;; Proposito:
;; pascal : <Int> N -> <List> R: Procedimiento encargado de retornar la
;; fila N del triangulo de pascal.
;;
;; <List> ::= ()
;;        ::= (<Scheme-Value> <List>)