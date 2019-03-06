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
(define numero_dorsal 2)



;;;;;;;;;;;;;;;;;;;;;;;;FUNCION PRINCIPAL;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
(define (CCCE2019 equipo_1 equipo_2)
  (cond ((or (> (suma_elementos equipo_1) 10) (> (suma_elementos equipo_2) 10))#f)
        (else
         (append (list 'E1 (hacer_equipos equipo_1))(list 'E2 (hacer_equipos equipo_2))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Funcion Hacer Equipos
;;E: Una lista que indica la formacion de un equipo de jugadores
;;S: Una lista con las estructuras de los jugadores
(define (hacer_equipos formacion)
  (cond((null? formacion) (list (hacer_portero)))
       ((equal? (length formacion) 3)
         (cons (hacer_bloque_defensivo (car formacion) (+ (suma_elementos_hasta formacion 1) 4))
               (hacer_equipos (cdr formacion))
               ))
       ((equal? (length formacion) 2)
        (cons (hacer_bloque_central(car formacion) (suma_elementos_hasta formacion 1)) (hacer_equipos (cdr formacion))))
       (else
        (cons (hacer_bloque_delantero(car formacion) numero_dorsal) (hacer_equipos (cdr formacion))))))
       



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
  (cond ((zero? cant_jugadores) '())
        (else
         (cons   (hacer_jugador_defensa dorsal)(hacer_bloque_defensivo (- cant_jugadores 1) (+ dorsal 1))))))

;;Funcion Bloque Central
;;E: cantidad de jugadores centrales por equipo
;;S: lista con los centralesf por equipo
(define (hacer_bloque_central cant_jugadores dorsal)
  (cond ((zero? cant_jugadores) '())
        (else
         (cons  (hacer_jugador_central dorsal) (hacer_bloque_central (- cant_jugadores 1)(+ dorsal 1))))))

;;Funcion Bloque Delantero
;;E: cantidad de jugadores delanteros por equipo
;;S: lista con los delanteros por equipo
(define (hacer_bloque_delantero cant_jugadores dorsal)
  (cond ((zero? cant_jugadores) '())
        (else
         (cons  (hacer_jugador_delantero dorsal) (hacer_bloque_delantero (- cant_jugadores 1) (+ dorsal 1))))))








;;;;;;;;;;;;;;;;;;;;;FUNCIONES AUXILIARES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (suma_elementos_hasta lista indice)
  (cond ((zero? indice) 0)
        (else
         ( + (suma_elementos_hasta_aux (car lista)) (suma_elementos_hasta (cdr lista) (- indice 1)))))) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (suma_elementos_hasta_aux cant)
  (cond ((zero? cant) 0)
        (else (+ 1 (suma_elementos_hasta_aux (- cant 1))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;/;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (suma_elementos lista)
  (cond ((null? lista) 0)
        (else
         ( + (suma_elementos_aux (car lista)) (suma_elementos (cdr lista)))))) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (suma_elementos_aux cant)
  (cond ((zero? cant) 0)
        (else (+ 1 (suma_elementos_aux (- cant 1))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;/;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Funcion Aleatoria
;;E: -
;;S: Un numero aleatorio
(define(numero-random)
  (random 0 20))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define(inversa lista)
  (cond ((null? lista) '())
        (else (append (inversa (cdr lista)) (list(car lista))))))