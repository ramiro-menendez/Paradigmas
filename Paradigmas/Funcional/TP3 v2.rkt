#lang racket

(define agenda-semanal
  '((lunes ("Francisco Perez" 8 8.5) ("Mariano López" 9 9.5))
    (martes ("José Álvarez" 10 11) ("Fernanda Martínez" 11 12))
    (miercoles) (jueves) (viernes)))

(define horarios-inicio
  '(8 8.5 9 9.5 10 10.5 11 11.5 14 14.5 15 15.5 16 16.5) )

;-----------------------------------------------------------------
;PARTE 1
;buscar-horario

(define buscar-horario
  (lambda (agenda tipo dia)
    (obtener-horario horarios-inicio tipo (obtener-cronograma agenda dia))))


;-----------------------------------------------------------------
;PARTE 2
;inserta-turno

(define inserta-turno
  (lambda(agenda nombre-persona tipo dia hora-inicio)
    (let ((cronograma (obtener-cronograma agenda dia)))
      (combina-cronograma-con-agenda agenda
                                     (inserta-turno-cronograma cronograma nombre-persona tipo hora-inicio)
                                     dia))))
    
(define inserta-turno-cronograma
  (lambda(cronograma nombre-persona tipo hora-inicio)
    (let ((hora-fin (if (equal? tipo 'estandar)
                        (+ hora-inicio 0.5)
                        (+ hora-inicio 1))))
      (cond ((turno-ocupado? cronograma hora-inicio tipo) 'turno-no-disponible)
            ((null? cronograma)(list (list nombre-persona hora-inicio hora-fin)))
            ((< (cadr (car cronograma)) hora-inicio)
             (append (list (car cronograma))
                     (inserta-turno-cronograma (cdr cronograma) nombre-persona tipo hora-inicio)))
            (else (append (list (list nombre-persona hora-inicio hora-fin)) (cdr cronograma)))))))      

(define combina-cronograma-con-agenda
  (lambda(agenda cronograma dia)
    (cond ((equal? dia 'lunes)    (append (list (append '(lunes) cronograma)) (cdr agenda)))
          ((equal? dia 'martes)   (append (list(car agenda))(list (append '(martes) cronograma))(cddr agenda)))
          ((equal? dia 'miercoles)(append (list(car agenda)(cadr agenda))(list (append '(miercoles) cronograma)) (cdddr agenda)))
          ((equal? dia 'jueves)   (append (list(car agenda)(cadr agenda)(caddr agenda))(list (append '(jueves) cronograma)) (cddddr agenda)))
          ((equal? dia 'viernes)  (append (list(car agenda)(cadr agenda)(caddr agenda)(cadddr agenda))(list (append '(viernes) cronograma)) (cdr(cddddr agenda)))))))
          
;-----------------------------------------------------------------
;PARTE 3

(define lista-huecos
  (lambda(agenda)
    (if (null? agenda)
        '()
        (cons (list (caar agenda) (lista-huecos-dia(filtrar-horarios horarios-inicio (obtener-cronograma agenda-semanal (caar agenda)))))
              (lista-huecos (cdr agenda))))))

;esto junta en sublistas los horarios libres seguidos
;el reverse es para "contrarrestar" el hecho que las listas se construyen comenzando desde el final        

(define (crear-intervalos horarios current-interval intervalos)
  (cond ((null? horarios)(reverse (if (null? current-interval)
                                      intervalos
                                      (cons (reverse current-interval) intervalos))))
        (else (if (or (null? current-interval)
                      (= (car horarios) (+ 0.5 (car current-interval))))
                  (crear-intervalos (cdr horarios) (cons (car horarios) current-interval) intervalos)
                  (crear-intervalos (cdr horarios) (list (car horarios)) (cons (reverse current-interval) intervalos))))))


;esto deja las sublistas con las puntas (elimina los del medio)
(define lista-huecos-dia
  (lambda(horarios)   
    (map (lambda(intervalo)
           (list (car intervalo) (+ 0.5 (car (reverse intervalo)))))
         (crear-intervalos horarios '() '()))))

;filtra los horarios libres con los ocupados
(define filtrar-horarios
  (lambda(horarios cronograma)
    (filter (lambda(x) (not(turno-ocupado? cronograma x 'estandar))) horarios)))

;-----------------------------------------------------------------
;PARTE 4
(define reagendar-dia
  (lambda(agenda-semanal


;-----------------------------------------------------------------
;obtener-horario

(define obtener-horario
  (lambda (horarios-posibles tipo cronograma-dia)
    (if (null? horarios-posibles)
        ('horario-no-disponible.)
        (if (turno-ocupado? cronograma-dia (car horarios-posibles) tipo)
            (obtener-horario (cdr horarios-posibles) tipo cronograma-dia)
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

; (En el horario pedido el consultorio está cerrado) ∨
;                    ¬(cronograma vacío) ∧
;                     [(horarioDado = horarioInicioTurno)  ∨
; si es estandar        (horarioDado - 0.5 = horarioInicioTurno) ∧ (horarioDado + 0.5 = horarioFinTurno)
; si es complejo        ((horarioDado + 0.5 = horarioInicioTurno) ∨ (horario = 11.5) ∨ (horario = 16.5))
;                       (turno-ocupado? (cuerpo cronograma))] 


(define turno-ocupado?
  (lambda(cronograma-dia horario tipo)
    (or (or (or (< horario 8)(> horario 16.5))
            (and (> horario 11.5) (< horario 14)))
        (and (not(null? cronograma-dia))
             (or (or (= horario (cadr (car cronograma-dia)))
                     (if (equal? tipo 'estandar)                                ;if
                         (and (= (- horario 0.5) (cadr (car cronograma-dia)))   ;then (si es estandar)
                              (= (+ horario 0.5) (caddr (car cronograma-dia))))
                         (or (= (+ horario 0.5) (cadr (car cronograma-dia)))    ;else (si es complejo)
                             (= horario 11.5)
                             (= horario 16.5))))       
                 (turno-ocupado? (cdr cronograma-dia) horario tipo))))))










