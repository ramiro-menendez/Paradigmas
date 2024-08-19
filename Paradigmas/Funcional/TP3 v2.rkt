#lang racket

(define agenda-semanal
  '((lunes ("Francisco Perez" 8 8.5) ("Mariano López" 9 9.5))
    (martes ("José Álvarez" 10 11) ("Fernanda Martínez" 11 12))
    (miercoles) (jueves) (viernes ("Ramiro Menendez" 15 15.5))))

(define agenda-semanal-1
  '((lunes ("Fabián" 8 9) ("Laura" 9 9.5) ("Javier" 10 11) ("Cristian" 11 12) ("Cesira" 15 16) ("Paula" 16 16.5) ("L7" 16.5 17))
    (martes ("Damián" 15.5 16) ("Milagros" 16 17))
    (miercoles) (jueves ("Martina" 8.5 9)) (viernes ("Ramiro Menendez" 15.5 16))))

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
    (lista-huecos-2 agenda agenda)))

(define lista-huecos-2
  (lambda(agenda agenda-completa)
    (if (null? agenda)
        '()
        (cons (list (caar agenda) (lista-huecos-dia(filtrar-horarios horarios-inicio (obtener-cronograma agenda-completa (caar agenda)))))
              (lista-huecos-2 (cdr agenda) agenda-completa)))))

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
  (lambda(agenda dia)
    (limpiar-dia-ocupado agenda dia (obtener-cronograma agenda dia))))

;esto arma la nueva agenda insertando el cronograma del día a reagendar en el día que corresponde
;si es viernes, llama a "nueva-agenda"

(define limpiar-dia-ocupado
  (lambda(agenda dia cronograma)
    (cond ((null? agenda) '())
          ((equal? dia (caar agenda)) (append (list(list(caar agenda))
                                                   (cons (caar(cdr agenda)) (rearmar-dia (cdr(cadr agenda)) cronograma horarios-inicio)))
                                              (limpiar-dia-ocupado (cddr agenda) dia cronograma)))
          ((equal? dia 'viernes) (nueva-agenda cronograma))
          (else (cons (car agenda) (limpiar-dia-ocupado (cdr agenda) dia cronograma))))))

;se le pasa el cronograma existente y el que le queremos meter, junto con todos los horarios
;devuelve un cronograma fusionado.

(define rearmar-dia
  (lambda(cronograma-existente cronograma-dado horarios-posibles)
    (cond ((and (null? horarios-posibles)
                ;(not (null? cronograma-existente))
                (not (null? cronograma-dado)))
           (error "'falla-asignacion" (cons (cadr (car cronograma-dado)) '())))
          ((and (null? cronograma-existente)
                (null? cronograma-dado)) '())
        ;  ((null? cronograma-existente) rearmar-dia "(append cronograma-dado '())")
          ((null? cronograma-dado) (append cronograma-existente '()))
          (else (let ([tipo (if (= (+ 1 (cadr(car cronograma-dado))) (caddr(car cronograma-dado)))
                                'complejo
                                'estandar)])
                  (if (not(turno-ocupado? cronograma-existente (car horarios-posibles) tipo))
                      (cons (list (caar cronograma-dado) (car horarios-posibles) (if (equal? tipo 'complejo)
                                                                                     (+ 1 (car horarios-posibles))
                                                                                     (+ 0.5 (car horarios-posibles))))
                            (rearmar-dia cronograma-existente (cdr cronograma-dado) (cdr horarios-posibles)))

                      (if (= (car horarios-posibles) (cadr(car cronograma-existente)))
                          (cons (car cronograma-existente) (rearmar-dia (cdr cronograma-existente) cronograma-dado
                                                                        (cdr horarios-posibles)))
                          (rearmar-dia cronograma-existente cronograma-dado (cdr horarios-posibles)))))))))


                   
                                                        
         

;esto arma una nueva agenda semanal con los horarios en el lunes
;(los reagendados del viernes)
(define nueva-agenda
  (lambda(cronograma)
    (cons (append (list 'lunes) cronograma)
          (cons (list 'martes) 
                (cons (list 'miercoles)
                      (cons (list 'jueves)
                            (cons (list 'viernes) '())))))))

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
          (else (error "ta mal")))))

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