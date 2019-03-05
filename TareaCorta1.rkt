;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tarea Corta 1
;; CCCE2019
;; Mariano Munioz Masis, Karla Rivera Sanchez, Daniel Prieto Sibaja
;; 2016121607, 2016100425, 2016XXXXXX
;;

;;NO VEO A NADIE HACIENDO CAMBIOS xd

#|
Estructura del Jugador

(# habilidad fuerza (pos inicial) (pos final) velocidad aptitud tipo)

Estructura del balon
((pos inicial) (pos final) velocidad)
|#
;;Indice de Funciones
;;#1 
;;#2
;;#3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;CONSTANTES
(define numeros '(0 1 2 3 4 5 6 7 8 9 10))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define(numero-random)
  (list-ref numeros (random (length numeros))))

(define(hacer_portero)
  (list '1 (numero-random) (numero-random) (numero-random) (numero-random) (numero-random) '0 'portero)) 

(define(hacer_j tipo)
  (list '1 (numero-random) (numero-random) (numero-random) (numero-random) (numero-random) '0 tipo)) 

(define (hacer_equipos formacion)
  (cond((null? formacion) (list (hacer_portero)))
       ((equal? (length formacion) 3) (list (hacer_bloque(- (car formacion)1) 'defensa) (hacer_equipos (cdr formacion))))
       ((equal? (length formacion) 2) (list (hacer_bloque(- (car formacion)1) 'medio_campista) (hacer_equipos (cdr formacion))))
       (else
        (list (hacer_bloque(- (car formacion)1) 'delantero) (hacer_equipos (cdr formacion))))))

(define (hacer_bloque cant_jugadores tipo)
  (cond ((zero? cant_jugadores) (hacer_j tipo))
        (else
         (list  (hacer_j tipo) (hacer_bloque (- cant_jugadores 1) tipo)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (obtener_fuerza))

(define (obtener_pos_inicial))

(define (obtener_pos_final))

(define (obtener_habilidad))

(define (obtener_aptitud))

(define (obtener_tipo))



(define (CCCE2019 equipo_1 equipo_2 generaciones_geneticas))

