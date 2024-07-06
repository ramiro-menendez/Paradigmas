#lang racket

(define agenda-semanal
  '((lunes ("Francisco Perez" 8 8.5) ("Mariano López" 9 9.5))
    (martes ("José Álvarez" 10 11) ("Fernanda Martínez" 11 12))
    (miercoles) (jueves) (viernes)))

(define horarios-inicio
  '(8 8.5 9 9.5 10 10.5 11 11.5 14 14.5 15 15.5 16 16.5) )

;-----------------------------------------------------------------
;buscar-horario

(define buscar-horario
  (lambda (agenda tipo dia)
    (obtener-horario horarios-inicio tipo (obtener-cronograma agenda dia))))


;-----------------------------------------------------------------
;inserta-turno

(define inserta-turno
  (lambda(agenda nombre-persona tipo dia hora-inicio)
    (let ((cronograma (obtener-cronograma agenda dia)))
      (inserta-turno-cronograma cronograma nombre-persona tipo hora-inicio))))
    
(define inserta-turno-cronograma
  (lambda(cronograma nombre-persona tipo hora-inicio)
    (let ((hora-fin (if (equal? tipo 'estandar)
                        (+ hora-inicio 0.5)
                        (+ hora-inicio 1))))
          (cond ((turno-ocupado? cronograma hora-inicio tipo) 'turno-no-disponible)
                ((null? cronograma)(list nombre-persona hora-inicio hora-fin))
                ((< (cadr (car cronograma)) hora-inicio)
                 (append (car cronograma)
                         (inserta-turno-cronograma (cdr cronograma) nombre-persona tipo hora-inicio)))
                (else (append (list (list nombre-persona hora-inicio hora-fin)) (cdr cronograma)))))))
    
          
<
          
    
;-----------------------------------------------------------------
;obtener-horario

(define obtener-horario
  (lambda (horarios-posibles tipo dia)
    (if (null? horarios-posibles)
        ('horario-no-disponible.)
        (if (turno-ocupado? dia (car horarios-posibles) tipo)
            (obtener-horario (cdr horarios-posibles) tipo dia)
            (car horarios-posibles)))))


;-----------------------------------------------------------------
;obtener-cronograma devuelve el cronograma del día que le pasamos.
    
(define obtener-cronograma
  (lambda (agenda dia)
    (cond ((equal? 'lunes dia) (cdr (car agenda)))
          ((equal? 'martes dia) (cdr (cadr agenda)))
          ((equal? 'miercoles dia) (cdr (caddr agenda)))
          ((equal? 'jueves dia) (cdr (cadddr agenda)))
          ((equal? 'viernes dia) (cdr (car (cddddr agenda))))
          (else (error "ta mal"))
          )
    )
  )

;----------------------------------------------------------------------
;turno-ocupado? devuelve #f si se puede reservar y #t si no es posible.

;                    ¬(cronograma vacío) ∧
;                     [(horarioDado = horarioInicioTurno)  ∨
; si es estandar        (horarioDado - 0.5 = horarioInicioTurno) ∧ (horarioDado + 0.5 = horarioFinTurno)
; si es complejo        ((horarioDado + 0.5 = horarioInicioTurno) ∨ (horario = 11.5) ∨ (horario = 16.5))
;                       (turno-ocupado? (cuerpo cronograma))] 


(define turno-ocupado?
  (lambda(cronograma-dia horario tipo)
    (and (not(null? cronograma-dia))
         (or (or (= horario (cadr (car cronograma-dia)))
                 (if (equal? tipo 'estandar)                                ;if
                     (and (= (- horario 0.5) (cadr (car cronograma-dia)))   ;then (si es estandar)
                          (= (+ horario 0.5) (caddr (car cronograma-dia))))
                     (or (= (+ horario 0.5) (cadr (car cronograma-dia)))    ;else (si es complejo)
                         (= horario 11.5)
                         (= horario 16.5))))       
             (turno-ocupado? (cdr cronograma-dia) horario tipo)))))










