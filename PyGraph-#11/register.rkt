#lang eopl

; Definir una primitiva para crear un registro vacío
(define (crear-registro)
  '())

; Definir una primitiva para acceder a un campo de un registro
(define (registro-ref registro campo)
  (let ((campo-pareja (assq campo registro)))
    (if campo-pareja
        (cdr campo-pareja)
        (eopl:error "Campo no encontrado" campo))))

; Definir una primitiva para establecer un campo de un registro
(define (registro-set! registro campo valor)
  (let loop ((reg registro) (nuevo-reg '()))
    (if (null? reg)
        (cons (cons campo valor) nuevo-reg) ; Añadir el nuevo campo al final
        (let ((par (car reg)))
          (if (eq? (car par) campo)
              ; Campo encontrado, actualizar el valor
              (append nuevo-reg (cons (cons campo valor) (cdr reg)))
              ; Campo no encontrado, continuar con el siguiente
              (loop (cdr reg) (cons par nuevo-reg)))))))

; Prueba de las primitivas
(let ((mi-registro (crear-registro)))
  ; Establecer campos
  (set! mi-registro (registro-set! mi-registro 'juan 30))
  (set! mi-registro (registro-set! mi-registro 'carlos 20))
  (set! mi-registro (registro-set! mi-registro 'casa "nueva"))
  
  ; Acceder a los campos
  (display (registro-ref mi-registro 'juan)) ; => 30
  (newline)
  (display (registro-ref mi-registro 'carlos)) ; => 20
  (newline)
  (display (registro-ref mi-registro 'casa)) ; => "nueva"
  (newline)

  ; Modificar un campo existente
  (set! mi-registro (registro-set! mi-registro 'juan 35))
  (display (registro-ref mi-registro 'juan)) ; => 35
  (newline)

  ; Añadir un nuevo campo
  (set! mi-registro (registro-set! mi-registro 'nuevo-campo "valor"))
  (display (registro-ref mi-registro 'nuevo-campo)) ; => "valor"
  (newline)

  ; Mostrar el registro completo
  (display mi-registro)) ; => '((nuevo-campo . "valor") (carlos . 20) (casa . "nueva") (juan . 35))