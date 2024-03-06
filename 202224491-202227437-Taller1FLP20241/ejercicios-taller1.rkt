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

;-----------------------------------
;--------EJEMPLOS DE PRUEBA---------
;-----------------------------------

(invert '())
;; Resultado esperado: ()

(invert '((a 1) (b 2) (c 3)))
;; Resultado esperado: ((1 a) (2 b) (3 c))

(invert '(("hola" "mundo") ("foo" "bar")))
;; Resultado esperado: (("mundo" "hola") ("bar" "foo"))

(invert '(('(e s) "racket") ("genial" "muy") (17 29) (81 'o)))
;; Resultado esperado: (("racket" (e s)) ("muy" "genial") (29 17) ('o 81))

;; ---------------------------------------------------------- ;;
;; Ejercicio 2
;; down:
;; Proposito:
;; down : <List> L -> <List> L':
;; Procedimiento que retorna una lista de cada elemento de L asociado a
;; un nivel mas de parentesis comparado con su estado original en L.
;;
;; <List> ::= ()
;;        ::= (<Scheme-Value> <List>)

(define down
  (lambda (remaining_list)
    (if (null? remaining_list)
        empty
        (cons (cons (car remaining_list) empty) (down (cdr remaining_list)) )
     )
  )
)
  

;-----------------------------------
;--------EJEMPLOS DE PRUEBA---------
;-----------------------------------

(down '())
;; Resultado esperado: '()

(down '(1 2 3 4))
;; Resultado esperado: '((1) (2) (3) (4))

(down '((1 2) (3 4) a b))
;; Resultado esperado: '(((1 2)) ((3 4)) (a) (b))

(down '(a (a (a b)) a))
;; Resultado esperado: '((a) ((a (a b))) (a))

;; ---------------------------------------------------------- ;;
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
    (cond
      [(null? lst) (if (= n 0) (list x) '())]
      [(= n 0) (cons x (cdr lst))]
      [else (cons (car lst) (list-set (cdr lst) (- n 1) x))]
      )
    )
  )

;-----------------------------------
;--------EJEMPLOS DE PRUEBA---------
;-----------------------------------

(list-set '() 0 'x)
;; Resultado esperado: (x)

(list-set '(1 2 3 4 5) 2 '(a b))
;; Resultado esperado: (1 2 (a b) 4 5)

(list-set '(a b c d) 3 'x)
;; Resultado esperado: (a b c x)

(list-set '(b b '(1 2) d) 2 '(a k o))
;; Resultado esperado: (b b (a k o) d)

;; ---------------------------------------------------------- ;;
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

(define filter-in
  (lambda (a-list predicate)

    (if (null? a-list)
        '()
        (if (predicate (car a-list))
          (cons (car a-list) (filter-in (cdr a-list) predicate))
          (filter-in (cdr a-list) predicate)
          )
        ))
)

;-----------------------------------
;--------EJEMPLOS DE PRUEBA---------
;-----------------------------------

(filter-in '() even?)
;; Resultado esperado: '()

(filter-in '(1 2 3 4 5 6) even?)
;; Resultado esperado: '(2 4 6)

(filter-in '(1 2 3 4 5 6) odd?)
;; Resultado esperado: '(1 3 5)

(filter-in '(a 1 b "string" 2 c) symbol?)
;; Resultado esperado: '(a b c)

(filter-in '(a "hello" b "world" c) string?)
;; Resultado esperado: '("hello" "world")

(filter-in '(1 "two" 3 'four 5) number?)
;; Resultado esperado: '(1 3 5)


;; ---------------------------------------------------------- ;;
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

;-----------------------------------
;--------EJEMPLOS DE PRUEBA---------
;-----------------------------------

(mix '() '())
;; Resultado esperado: ()

(mix '() '(1 2 3))
;; Resultado esperado: ()

(mix '(a b c) '(1 2 3))
;; Resultado esperado: (a 1 b 2 c 3)

(mix '(a "hello" 3) '(1 'b "world"))
;; Resultado esperado: (a 1 "hello" 'b 3 "world")

;; ---------------------------------------------------------- ;;
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

(define swapper
  (lambda (e1 e2 a-list)
    (cond
      [(null? a-list) '()]
      [(equal? e2 (car a-list)) (cons e1 (swapper e1 e2 (cdr a-list)))]
      [(equal? e1 (car a-list)) (cons e2 (swapper e1 e2 (cdr a-list)))]
      [else (cons (car a-list) (swapper e1 e2 (cdr a-list)))])
  )
)

;-----------------------------------
;--------EJEMPLOS DE PRUEBA---------
;-----------------------------------

(swapper 1 'one '(1 2 3 one 4 one 1))
;; Resultado esperado: '(one 2 3 1 4 1 one)

(swapper 'a 'b '(a b a b c a b))
;; Resultado esperado: '(b a b a c b a)

(swapper 'a 'z '(a b c a d))
;; Resultado esperado: '(z b c z d)


;; ---------------------------------------------------------- ;;
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

;-----------------------------------
;--------EJEMPLOS DE PRUEBA---------
;-----------------------------------

(cartesian-product '() '())
;; Resultado esperado: ()

(cartesian-product '() '(x y z))
;; Resultado esperado: ()

(cartesian-product '(a b) '())
;; Resultado esperado: ()

(cartesian-product '(p q r) '(5 6 7))
;; Resultado esperado: ((p 5) (p 6) (p 7)
;;(q 5) (q 6) (q 7) (r 5) (r 6) (r 7))

(cartesian-product '(a b c) '(x))
;; Resultado esperado: ((a x) (b x) (c x))


;; ---------------------------------------------------------- ;;
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

(define mapping 
  (lambda (f l1 l2)
    (cond
      [(or (null? l1) (null? l2)) '()]
      [(equal? (f (car l1)) (car l2)) (cons (list (car l1) (car l2)) (mapping f (cdr l1) (cdr l2)))]
      [else (mapping f (cdr l1) (cdr l2))]
      )
  )
)

;-----------------------------------
;--------EJEMPLOS DE PRUEBA---------
;-----------------------------------

(mapping (lambda (x) (* x x)) '() '())
;; Resultado esperado: '()

(mapping (lambda (x) (+ x 3)) '(1 2 3) '(4 5 7))
;; Resultado esperado: '((1 4) (2 5))

(mapping (lambda (x) (* x 2)) '(1 2 3) '(2 4))
;; Resultado esperado: '((1 2) (2 4))

(mapping (lambda (x) (string-append (number->string x) "s")) '(1 2 3 4) '("1s" "2s" "3s"))
;; Resultado esperado: '((1 "1s") (2 "2s") (3 "3s"))


;; ---------------------------------------------------------- ;;
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

;-----------------------------------
;--------EJEMPLOS DE PRUEBA---------
;-----------------------------------

(reverse '())
;; Resultado esperado: '()

(reverse '(3 "hello" a (1 2) b))
;; Resultado esperado: '(b (1 2) a "hello" 3)

(reverse '(2 3 8 6 1))
;; Resultado esperado: '(1 6 8 3 2)

(reverse '(1 "dos" 3 'cuatro))
;; Resultado esperado: '(cuatro 3 "dos" 1)

;; ---------------------------------------------------------- ;;
;; Ejercicio 10
;; flatten:
;; Proposito:
;; flatten : <List> L -> <List> L': Procedimiento encargado de retornar
;; la lista "aplanada" que le entro como parametro, es decir sin ningun
;; tipo de lista anidada interna conservando todos los elementos.
;;
;; <List> ::= ()
;;        ::= (<Scheme-Value> <List>)

(define flatten
  (lambda (l)
   (if (null? l) '()
    (if (list? l)
      (append (flatten (car l)) (flatten (cdr l)))
      (cons l empty)
     )
   )
 )
)
  
;-----------------------------------
;--------EJEMPLOS DE PRUEBA---------
;----------------------------------- 

(flatten '())
;; Resultado esperado: '()

(flatten '(1 (2 3) 4))
;; Resultado esperado: '(1 2 3 4)

(flatten '(1 () 2 (3 ()) 4))
;; Resultado esperado: '(1 2 3 4)

(flatten '(a (b (c d) e) f))
;; Resultado esperado: '(a b c d e f)

;; ---------------------------------------------------------- ;;
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

;-----------------------------------
;--------EJEMPLOS DE PRUEBA---------
;-----------------------------------

(unzip '())
;; Resultado esperado: '(() ())

(unzip '((1 2) (3 4) (5 6)))
;; Resultado esperado: '((1 3 5) (2 4 6))

(unzip '((a "hello") (b "world") (c "!")))
;; Resultado esperado: '((a b c) ("hello" "world" "!"))

(unzip '(((1 2) a) ((3 4) b) ((5 6) c)))
;; Resultado esperado: '(((1 2) (3 4) (5 6)) (a b c))

;; ---------------------------------------------------------- ;;
;; Ejercicio 12
;; scan:
;; Proposito:
;; scan : <List> L <Int> n <lambda-binary> F -> <List> L':
;; Procedimiento encargado de tomar el elemento n y aplicar la
;; funcion binaria F con cada elemento de la lista de forma acomulativa
;; retornando una lista con cada resultado parcial empezando con el
;; elemento n.
;;
;; <lambda-binary> ::= (lambda (<identificador> <identificador>) <expresion>)
;;
;; <List> ::= ()
;;        ::= (<Scheme-Value> <List>)

(define scan
  (lambda (L n F)
    (if (null? L)
        (list n)  
        (let ((new-n (F n (car L)))) 
          (cons n  
                (scan (cdr L) new-n F)
                )
          )
        )
    )
  )

;-----------------------------------
;--------EJEMPLOS DE PRUEBA---------
;-----------------------------------

(scan '() 10 +)
;; Resultado esperado: '(10)

(scan '(1 2 3) 10 -)
;; Resultado esperado: '(10 9 7 4)

(scan '(2 4) 8 /)
;; Resultado esperado: '(8 4 1)

;; ---------------------------------------------------------- ;;
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

;-----------------------------------
;--------EJEMPLOS DE PRUEBA---------
;-----------------------------------

(operate (list +) '(3 4))
;; Resultado esperado: 7

(operate (list + * -) '(2 3 4 5))
;; Resultado esperado: 15

(operate (list / - +) '(12 4 3 6))
;; Resultado esperado: 6

(operate (list + * - /) '(5 6 3 4 2))
;; Resultado esperado: 14.5

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

(define path
  (lambda (n A)
    (cond [(null? A) empty]
          [else (cond
                  [(= n (car A)) empty]
                  [(< n (car A)) (cons 'left (path n (cadr A)))]
                  [(> n (car A)) (cons 'right (path n (caddr A)))]
                  )
                ]
          )
    )
  )

;-----------------------------------
;--------EJEMPLOS DE PRUEBA---------
;-----------------------------------

(path 17 '())
;; Resultado esperado: '()

(path 14 '(14 (7 () ()) (26 () ())))
;; Resultado esperado: '()

(path 1 '(8 (3 (1 () ()) (6 () ())) (10 () ())))
;; Resultado esperado: '(left left)

(path 31 '(14 (7 () (12 () ())) (26 (20 (17 () ())) (31 () ()))))
;; Resultado esperado: '(right right)

(path 1 '(8 (3 (1 () ()) (6 () ())) (10 () ())))
;; Resultado esperado: '(left left)

;; ---------------------------------------------------------- ;;
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

;-----------------------------------
;--------EJEMPLOS DE PRUEBA---------
;-----------------------------------

(inorder '())
;; Resultado esperado: '()

(inorder '(10 (5 (2 () ()) (8 () ())) (15 (12 () ()) (20 () ()))))
;; Resultado esperado: '(2 5 8 10 12 15 20)

(inorder '(42 () ()))
;; Resultado esperado: '(42)

;; ---------------------------------------------------------- ;;
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

(define o-b-helper
  (lambda (element value-1 value-2)
    (cond
      [(eqv? element 'suma) (+ value-1 value-2)]
      [(eqv? element 'resta) (- value-1 value-2)]
      [(eqv? element 'multiplica) (* value-1 value-2)]
      )
    )
  )

(define Operar-binarias
  (lambda (operacionB)
    (if (number? operacionB)
      operacionB  
      (let ((op (cadr operacionB))
            (val-1 (Operar-binarias (car operacionB)))
            (val-2 (Operar-binarias (caddr operacionB))))
        (o-b-helper op val-1 val-2)
        )
      )
    )
  )

;-----------------------------------
;--------EJEMPLOS DE PRUEBA---------
;-----------------------------------

(Operar-binarias '((3 resta (4 multiplica 1))
                   multiplica ((2 resta 3) suma 11)))
;; Resultado esperado: -10

(Operar-binarias '((10 resta 2) multiplica (1 suma 3)))
;; Resultado esperado: 32

(Operar-binarias '(3 suma 4))
;; Resultado esperado: 7

(Operar-binarias '(5 resta 3))
;; Resultado esperado: 2

(Operar-binarias '(4 multiplica -5))
;; Resultado esperado: -20

;; ---------------------------------------------------------- ;;
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

;-----------------------------------
;--------EJEMPLOS DE PRUEBA---------
;-----------------------------------

(prod-scalar-matriz '((1 2) (3 4)) '(5 6))
;; Resultado esperado: '((5 12) (15 24))

(prod-scalar-matriz '() '())
;; Resultado esperado: '()

(prod-scalar-matriz '((1) (2)) '(0))
;; Resultado esperado: ((0) (0))

(prod-scalar-matriz '((1 2 3) (4 5 6)) '(7 8 9))
;; Resultado esperado: '((7 16 27) (28 40 54))


;; Ejercicio 18
;; pascal:
;; Proposito:
;; pascal : <Int> N -> <List> R: Procedimiento encargado de retornar la
;; fila N del triangulo de pascal.
;;
;; <List> ::= ()
;;        ::= (<Scheme-Value> <List>)