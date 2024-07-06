#lang scheme

(define agenda-semanal
  '((lunes ("Francisco Perez" 8 8.5) ("Mariano López" 9 9.5))
    (martes ("José Álvarez" 10 11) ("Fernanda Martínez" 11 12))
    (miercoles) (jueves) (viernes)))


"(define buscar-horario
  (lambda (agenda tipo dia)
    (
     (if ()
         'horario-no-disponible

         
         )
     )
    )
  )"


(define buscar-dia
  (lambda(agenda dia)
    (cond ((equal? 'lunes dia) (car agenda))
          ((equal? 'martes dia) (cadr agenda))
          ((equal? 'miercoles dia) (caddr agenda))
          ((equal? 'jueves dia) (cadddr agenda))
          ((equal? 'viernes dia) (car (cddddr agenda)))
          (else (error "Error en la búsqueda del día solicitado:
Los días válidos son desde lunes hasta viernes.
Verifique que lo haya ingresado correctamente y en minúsculas
Ejemplo -> 'martes"))
          )
    )
  )
                                 

(define turno-complejo?
  (lambda (turno)
    (if(= 1 (- (caddr turno) (cadr turno)))
       'complejo
       'estandar
       )
    )
  )