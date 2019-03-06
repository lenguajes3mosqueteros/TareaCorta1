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

(dorsal habilidad fuerza velocidad (pos inicial) (pos final)  aptitud tipo)

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



;;;;;;;;;;;;;;;;;;;;;;;;FUNCION PRINCIPAL;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
(define (CCCE2019 equipo_1 equipo_2 generaciones_geneticas)
  (list (hacer_equipos equipo_1)(hacer_equipos equipo_2)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Funcion Hacer Equipos
;;E: Una lista que indica la formacion de un equipo de jugadores
;;S: Una lista con las estructuras de los jugadores
(define (hacer_equipos formacion)
  (cond((null? formacion) (list (hacer_portero)))
       ((equal? (length formacion) 3)
        (list (hacer_bloque_defensivo(- (car formacion)1) numero_dorsal) (hacer_equipos (cdr formacion))))
       ((equal? (length formacion) 2)
        (list (hacer_bloque_central(- (car formacion)1) (+ (suma_elementos_lista formacion 1) numero_dorsal)) (hacer_equipos (cdr formacion))))
       (else
        (list (hacer_bloque_delantero(- (car formacion)1) (+ (suma_elementos_lista formacion 1) numero_dorsal)) (hacer_equipos (cdr formacion))))))



;;Funcion: Hacer Portero
;;E: -
;;S: la estructura de un jugador de tipo portero
(define(hacer_portero)
  (list '1 (numero-random) (numero-random) (numero-random) (numero-random) (numero-random) '0 'portero)) 


;;Funcion Hacer Delantero
;;E: numero de dorsal
;;S: Estructura de un delantero
(define(hacer_jugador_delantero dorsal)
  (list dorsal (numero-random) (numero-random) (numero-random) (numero-random) (numero-random) '0 'delantero))

;;Funcion Hacer Medio Campista
;;E: numero de dorsal
;;S: Estructura de un central
(define(hacer_jugador_central dorsal)
  (list dorsal (numero-random) (numero-random) (numero-random) (numero-random) (numero-random) '0 'medio_campista))

;;Funcion Hacer Defensa
;;E: numero de dorsal
;;S: Estructura de un Defensa
(define(hacer_jugador_defensa dorsal)
  (list dorsal (numero-random) (numero-random) (numero-random) (numero-random) (numero-random) '0 'defensa))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Funcion Bloque Defensivo
;;E: cantidad de jugadores defensa por equipo
;;S: lista con los defensas por equipo
(define (hacer_bloque_defensivo cant_jugadores dorsal)
  (cond ((zero? cant_jugadores) (hacer_jugador_defensa dorsal))
        (else
         (list  (hacer_jugador_defensa  dorsal) (hacer_bloque_defensivo (- cant_jugadores 1) (+ dorsal 1))))))

;;Funcion Bloque Central
;;E: cantidad de jugadores centrales por equipo
;;S: lista con los centralesf por equipo
(define (hacer_bloque_central cant_jugadores dorsal)
  (cond ((zero? cant_jugadores) (hacer_jugador_central dorsal))
        (else
         (list  (hacer_jugador_central dorsal) (hacer_bloque_central (- cant_jugadores 1)(+ dorsal 1))))))

;;Funcion Bloque Delantero
;;E: cantidad de jugadores delanteros por equipo
;;S: lista con los delanteros por equipo
(define (hacer_bloque_delantero cant_jugadores dorsal)
  (cond ((zero? cant_jugadores) (hacer_jugador_delantero dorsal))
        (else
         (list  (hacer_jugador_delantero dorsal) (hacer_bloque_delantero (- cant_jugadores 1) (+ dorsal 1))))))








;;;;;;;;;;;;;;;;;;;;;FUNCIONES AUXILIARES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (suma_elementos_lista lista indice)
  (cond ((zero? indice) 0)
        (else
         ( + (suma_elementos_lista_aux (car lista)) (suma_elementos_lista (cdr lista) (- indice 1)))))) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (suma_elementos_lista_aux cant)
  (cond ((zero? cant) 0)
        (else (+ 1 (suma_elementos_lista_aux (- cant 1))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Funcion Aleatoria
;;E: -
;;S: Un numero aleatorio
(define(numero-random)
  (list-ref numeros (random (length numeros))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




