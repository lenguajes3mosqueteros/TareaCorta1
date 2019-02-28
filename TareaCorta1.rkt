;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tarea Corta 1
;; CCCE2019
;; Mariano Munioz Masis, Karla Rivera Sanchez, Daniel Prieto Sibaja
;; 2016121607, 2016100425, 2016XXXXXX
;;


#|
Estructura del Jugador

(# habilidad fuerza (pos inicial) (pos final) habilidad aptitud tipo)

Estructura del balon
((pos inicial) (pos final) velocidad)
|#
;;Indice de Funciones
;;#1 
;;#2
;;#3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;VARIABLES
(define contador_formacion 0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (hacer_jugadores equipo)
    (cond 
        ((null? equipo) '())
        (else
            (hacer_jugadores_aux (car equipo) contador_formacion)
        )   
    )
)

(define (hacer_jugadores_aux cant_jugadores linea)
    (cond 
        ((equal? cant_jugadores 0) '())
        (else
            ()
        

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (obtener_fuerza))

(define (obtener_pos_inicial))

(define (obtener_pos_final))

(define (obtener_habilidad))

(define (obtener_aptitud))

(define (obtener_tipo))



(define (CCCE2019 equipo_1 equipo_2 generaciones_geneticas))

