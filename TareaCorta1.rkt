;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tarea Corta 1
;; CCCE2019
;; Mariano Munioz Masis, Karla Rivera Sanchez, Daniel Prieto Sibaja
;; 2016121607, 2016100425, 2016XXXXXX
;;

;;NO VEO A NADIE HACIENDO CAMBIOS
;;NECESITAMOS AVANZAR

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
(define numero_dorsal 2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define(numero-random)
  (list-ref numeros (random (length numeros))))

(define(hacer_portero)
  (list '1 (numero-random) (numero-random) (numero-random) (numero-random) (numero-random) '0 'portero)) 

(define(hacer_jugador_delantero dorsal)
  (list dorsal (numero-random) (numero-random) (numero-random) (numero-random) (numero-random) '0 'delantero))

(define(hacer_jugador_central dorsal)
  (list dorsal (numero-random) (numero-random) (numero-random) (numero-random) (numero-random) '0 'medio_campista))

(define(hacer_jugador_defensa dorsal)
  (list dorsal (numero-random) (numero-random) (numero-random) (numero-random) (numero-random) '0 'defensa))

(define (hacer_equipos formacion)
  (cond((null? formacion) (list (hacer_portero)))
       ((equal? (length formacion) 3)
        (list (hacer_bloque_defensivo(- (car formacion)1) numero_dorsal) (hacer_equipos (cdr formacion))))
       ((equal? (length formacion) 2)
        (list (hacer_bloque_central(- (car formacion)1) (+ (suma_elementos_lista formacion 1) numero_dorsal)) (hacer_equipos (cdr formacion))))
       (else
        (list (hacer_bloque_delantero(- (car formacion)1) (+ (suma_elementos_lista formacion 1) numero_dorsal)) (hacer_equipos (cdr formacion))))))

(define (hacer_bloque_defensivo cant_jugadores dorsal)
  (cond ((zero? cant_jugadores) (hacer_jugador_defensa dorsal ))
        (else
         (list  (hacer_jugador_defensa  dorsal) (hacer_bloque_defensivo (- cant_jugadores 1) (+ dorsal 1))))))

(define (hacer_bloque_central cant_jugadores dorsal)
  (cond ((zero? cant_jugadores) (hacer_jugador_central dorsal))
        (else
         (list  (hacer_jugador_central dorsal) (hacer_bloque_central (- cant_jugadores 1)(+ dorsal 1))))))

(define (hacer_bloque_delantero cant_jugadores dorsal)
  (cond ((zero? cant_jugadores) (hacer_jugador_delantero dorsal))
        (else
         (list  (hacer_jugador_delantero dorsal) (hacer_bloque_delantero (- cant_jugadores 1) (+ dorsal 1))))))


(define (suma_elementos_lista lista indice)
  (cond ((zero? indice) 0)
        (else
         ( + (suma_elementos_lista_aux (car lista)) (suma_elementos_lista (cdr lista) (- indice 1)))))) 
(define (suma_elementos_lista_aux cant)
  (cond ((zero? cant) 0)
        (else (+ 1 (suma_elementos_lista_aux (- cant 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (CCCE2019 equipo_1 equipo_2 generaciones_geneticas)
  (list (hacer_equipos equipo_1)(hacer_equipos equipo_2)))