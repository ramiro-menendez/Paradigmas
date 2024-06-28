#lang racket

;-------------A------------------
(define distancia
  (lambda (p1 p2)
    (sqrt
     (+
      (expt (- (car p2) (car p1)) 2)
      (expt (- (cadr p2) (cadr p1)) 2)
      )
     )
    )
  )

;--------------B--------------------
(define mas-cercano
  (lambda(P)
    (lambda(P1 P2)
      (> (distancia P1 P) (distancia P2 P)))))

;--------------C------------------

;Esta ordenada si...

;La lista tiene un sólo punto o la dist a P es menor en la cabeza
;que en el segundo elemento y el resto está ordenado.

(define ordenados?
  (lambda(p lista-puntos)
    (or
     (null? (cdr lista-puntos))
     (and
      ;(< (distancia p (car lista-puntos))
       ;  (distancia p (cadr lista-puntos)))
      ((mas-cercano p) (cadr lista-puntos) (car lista-puntos))
      (ordenados? p (cdr lista-puntos))
      )
     )
    )
  )


















  