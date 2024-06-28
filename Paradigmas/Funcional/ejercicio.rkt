#lang racket

;1 - factorial de un número

(define factorial
  (lambda(n)
    (if (= n 0) 1
        (* n (factorial (- n 1))))))

;2 - Máximo elemento de una lista

(define maximo
  (lambda(x)
    (cond ((null? x) (error "maximo: no admite lista vacia"))
          ((null? (cdr x)) (car x))
          (else
            (let [
                  (cabeza (car x))            ;define la cabeza como el 'Content of the Address part of the Register' de la lista.
                  (maxcola (maximo (cdr x)))  ;define el maxcola como el maximo del 'Content of the Decrement part of the Register' de la lista.
                  ]
              (if (> cabeza maxcola)
                  cabeza    ;then
                  maxcola)  ;else
             )
           ) ;cierra else
     ) ;cierra cond
   ) ;cierra lambda
  )

;3 - Longitud de una lista

(define longitud
  (lambda(x)
    (cond ((null? x) 0)
          ((null? (cdr x)) 1)
          (else
           (+ 1 (longitud (cdr x)))
           )
    )
 ))

;4 - Sumar los elementos de una lista.

(define sumatoria
  (lambda(x)
    (cond ((null? x) (error "Error: la lista no tiene elementos"))
          ((null? (cdr x)) (car x))
          (else
           (+ (car x) (sumatoria (cdr x)))
           )
      )
    )
 )

;5 - Determinar si un elemento es miembro de una lista

(define miembro?
  (lambda(n x)
    (if (null? x) #f
          (if(equal? n (car x)) #t
                                (miembro? n (cdr x))))))

      ;También se puede pensar en forma matemática, con booleanos
      ; ¬(lista vacía) ^ (está en la cabeza ∨ está en la cola)
(define miembrov2?
  (lambda(n x)
    (and
     (not (null? x))
     (or
      (equal? n (car x)) (miembrov2? n (cdr x))))))
  

;6 - Concatenar dos listas

(define concatenar
  (lambda(a b)
    (if (null? a) b
        (cons (car a) (concatenar(cdr a) b)))))

;7 - Hallar el n-ésimo elemento de una lista

(define n-esimo
  (lambda (n lista)
    (if (null? lista) (error "No hay suficientes elementos en la lista.")
        (if (= n 1) (car lista)
            (n-esimo (- n 1) (cdr lista))))))

;8 - Determinar si una lista es sublista de otra.

     ;Definición matemática usando booleanos:
     ; sublista? lista1 lista2 := negación(lista2 vacía) y ((lista1 está en la cabeza de la lista2) ó (lista1 está en el cuerpo de la lista2))

     ;Ahí digo: "una lista1 es sublista de una lista2 si se da lo siguiente -> la lista dos no está vacía y la lista1 está en la cabeza de la lista2
;                o en el cuerpo de esa lista2".
(define sublista?
  (lambda(lista1 lista2)
    (and
     (not(null? lista2))
     (or
      (equal? lista1 (car lista2))
      (sublista? lista1 (cdr lista2))))))

;Ejercicio extra - Determinar si un número es perfecto

(define perfect?
  (lambda(x)
    (if (= (sumatoriadivisores x) (doble x)) #t #f)))

(define doble
  (lambda(x)
    (* 2 x)))

(define sumatoriadivisores
  (lambda(x)
    (sumatoriadivisoresdesde x 1)))

(define sumatoriadivisoresdesde
  (lambda(x n)
    (cond
      ((> n x) 0)
      ((divisor? n x) (+ n (sumatoriadivisoresdesde x (+ n 1))))
      (else (sumatoriadivisoresdesde x (+ n 1))))))

(define divisor?
  (lambda(n x)
    (= (remainder x n) 0)))

;-------Árboles binarios---------

(define arbol-vacio '())

(define (crear-arbol raiz izq der)
  (list raiz izq der))

(define (crear-hoja raiz)
  (crear-arbol raiz arbol-vacio arbol-vacio))

(define (raiz arbol) (car arbol))
(define (izq arbol) (cadr arbol))
(define (der arbol) (caddr arbol))

(define (vacio? arbol)
  (eq? arbol arbol-vacio))

(define (es-hoja? arbol)
  (and (not (vacio? arbol))
       (vacio? (izq arbol))
       (vacio? (der arbol))))









