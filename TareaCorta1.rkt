;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tarea Corta 1
;; CCCE2019
;; Mariano Munioz Masis, Karla Rivera Sanchez, Daniel Prieto Sibaja
;; 2016121607, 2016100425, 2016072504
;;
;;Fixing
#|

;; Estructura completa
(E1
  (((2 13 6 4 (84 568) 17 0 defensa)
    (3 9 3 9 (226 587) 8 0 defensa)
    (4 17 18 2 (579 17) 5 0 defensa)
    (5 4 0 16 (160 588) 6 0 defensa))
   ((6 2 0 17 (589 385) 2 0 medio_campista)
    (7 10 6 2 (821 130) 14 0 medio_campista)
    (8 0 2 3 (304 60) 16 0 medio_campista)
    (9 7 0 18 (730 441) 17 0 medio_campista))
   ((10 3 3 1 (547 241) 15 0 delantero)
    (11 10 3 16 (623 214) 11 0 delantero))
   ((1 4 0 11 (460 540) 17 0 portero)))
  E2
  (((2 4 18 13 (20 261) 0 0 defensa)
    (3 5 9 0 (349 323) 5 0 defensa)
    (4 13 4 4 (907 10) 5 0 defensa)
    (5 15 1 16 (145 639) 0 0 defensa)
    (6 2 2 5 (334 487) 9 0 defensa))
   ((7 19 16 16 (410 42) 8 0 medio_campista)
    (8 16 6 19 (441 483) 7 0 medio_campista)
    (9 17 4 2 (477 612) 4 0 medio_campista))
   ((10 9 5 3 (590 40) 16 0 delantero)
    (11 12 18 1 (68 454) 2 0 delantero))
   ((1 8 16 17 (671 565) 12 0 portero))))
Estructura del Jugador
(dorsal habilidad fuerza velocidad (pos inicial) (pos final)  aptitud tipo)
(  0        1        2      3             4         5           6       7 )

Estructura del balon
((pos inicial) (pos final) velocidad)
|#
;;Indice de Funciones
;;#1 
;;#2
;;#3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket
;;CONSTANTES
(define largo_campo 1350)
(define ancho_campo 650) 
(define numero_dorsal 2)



;;;;;;;;;;;;;;;;;;;;;;;;FUNCION PRINCIPAL;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
(define (CCCE2019 equipo_1 equipo_2)
  (cond ((or (> (suma_elementos equipo_1) 10) (> (suma_elementos equipo_2) 10))#f)
        (else
         (append (list 'E1 (hacer_equipos equipo_1 numero_dorsal 1))(list 'E2 (hacer_equipos equipo_2 numero_dorsal 2))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Funcion Hacer Equipos
;;E: Una lista que indica la formacion de un equipo de jugadores
;;S: Una lista con las estructuras de los jugadores
(define (hacer_equipos formacion dorsal numero_equipo)
  (cond((null? formacion) (list(list (hacer_portero))))
       ((equal? (length formacion) 3)
         (cons (hacer_bloque_defensivo (car formacion) dorsal)
               (hacer_equipos (cdr formacion) (+ dorsal (car formacion))numero_equipo)))
       ((equal? (length formacion) 2)
        (cons (hacer_bloque_central(car formacion) dorsal)
              (hacer_equipos (cdr formacion) (+ dorsal (car formacion)) numero_equipo)))
       (else
        (cons (hacer_bloque_delantero(car formacion) dorsal)
              (hacer_equipos (cdr formacion) (+ dorsal (car formacion)) numero_equipo)))))
       



;;Funcion: Hacer Portero
;;E: -
;;S: la estructura de un jugador de tipo portero
(define(hacer_portero)
  (list '1 (numero-random) (numero-random) (numero-random) (posicion_random) (numero-random) '0 'portero)) 


;;Funcion Hacer Delantero
;;E: numero de dorsal
;;S: Estructura de un delantero
(define(hacer_jugador_delantero dorsal)
  (list dorsal (numero-random) (numero-random) (numero-random) (posicion_random) (numero-random) '0 'delantero))

;;Funcion Hacer Medio Campista
;;E: numero de dorsal
;;S: Estructura de un central
(define(hacer_jugador_central dorsal)
  (list dorsal (numero-random) (numero-random) (numero-random) (posicion_random) (numero-random) '0 'medio_campista))

;;Funcion Hacer Defensa
;;E: numero de dorsal
;;S: Estructura de un Defensa
(define(hacer_jugador_defensa dorsal)
  (list dorsal (numero-random) (numero-random) (numero-random) (posicion_random) (numero-random) '0 'defensa))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;Getters y Setters;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (obtener_dorsal lista) (elemento_en_posicion lista 0))
(define (obtener_posicion_inicial lista) (elemento_en_posicion lista 4))
(define (obtener_posicion_final lista) (elemento_en_posicion lista 5))
(define (obtener_velocidad lista) (elemento_en_posicion lista 3))
(define (obtener_tipo lista) (elemento_en_posicion lista 7))



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

;;Funcion que devuelve una posicion
;;E: -
;;S: Posicion random
(define (posicion_random)
  (list (random 0 1080)(random 0 680)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define(inversa lista)
  (cond ((null? lista) '())
        (else (append (inversa (cdr lista)) (list(car lista))))))

;;Funcion Devuelve en posicion.
;;E: lista y una posicion en lista
;;S: elemento de la posiscion
(define (elemento_en_posicion lista posicion)
  (cond ((< (length lista) posicion) #f)
        (else (elemento_en_posicion_aux lista posicion 0))))
(define(elemento_en_posicion_aux lista posicion indice)
  (cond ((equal? posicion indice) (car lista))
        (else (elemento_en_posicion_aux (cdr lista) posicion (+ 1 indice)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (promedio_especial listaA listaB)
  (promedio_especial_aux listaA listaB 0))
(define (promedio_especial_aux listaA listaB indice)
  (cond ((equal? 8 indice) '())
        ((equal? 7 indice) (cons (obtener_tipo listaA) (promedio_especial_aux listaA listaB (+ indice 1))))
        ((equal? 5 indice) (cons (nueva_pos (obtener_posicion_inicial listaA)(obtener_posicion_inicial listaB)) (promedio_especial_aux listaA listaB (+ indice 1))))
        ((equal? 4 indice) (cons (obtener_posicion_final listaA) (promedio_especial_aux listaA listaB (+ indice 1))))
        ((zero? indice ) (cons (obtener_dorsal listaA) (promedio_especial_aux listaA listaB (+ indice 1))))
        (else
         (cons (quotient (+ (elemento_en_posicion listaA indice)(elemento_en_posicion listaB indice)) 2) (promedio_especial_aux listaA listaB (+ indice 1))))))

(define (nueva_pos listaA listaB)
  (cond ((null? listaA) '())
        (else
         (cons (quotient (+(car listaA)(car listaB))2) (nueva_pos (cdr listaA) (cdr listaB))))))

(define (seleccionar_azar lista)
  (seleccionar_azar_aux lista 0 (hacer_lista_random (random 0 4))))
;;Algoritmo Genético, hagamos pruebas
(define(sum lista)
  (cond((null? lista)
        0)
       (else
        (+ (car lista) (sum (cdr lista))))))

(define(promedio lista)
  (cond((null? lista)
        0)
       (else
        (/ (sum lista)  (length lista)))))

(define(cuadrado num)
  (* num num))

(define(hipotenusa a b)
  (cond((zero? a)
        (cuadrado b))
       ((zero? b)
        (cuadrado a))
       (else
        (+ (cuadrado a) (cuadrado b)))))

(define(elemento index lista)
  (cond((null? lista)
        '())
       ((equal? index 1)
        (car lista))
       (else
        (elemento (- index 1) (cdr lista)))))

(define(mayor ele lista)
  (cond((null? lista)
        ele)
       ((< ele (car lista))
        (mayor (car lista) (cdr lista)))
       (else
        (mayor ele (cdr lista)))))

(define(aplicar fun lista)
  (cond((null? lista)
        '())
       (else
        (cons (fun (car lista)) (aplicar fun (cdr lista))))))

(define(reproduccion parentA parentB)
  (cond((null? parentA)
        parentB)
       ((null? parentB)
        parentA)
       (else
        (append(promedio_especial parentA (car parentB))(reproduccion parentA (cdr parentB))))))

(define(aptitud lista)
  (cond((null? lista)
        0)
       (else
        (append(aptitud-aux(car lista))(aptitud (cdr lista))))))

(define(aptitud-aux)
  (cond((null? lista)
        0)
       (else
        (promedio lista))))

(define(seleccion lista)
  (cond((null? lista)
        0)
       (else
        (mayor(aptitud lista)))))

(define(mutacion lista)
  (cond((null? lista)
        lista)
       (else
        (mutacion-aux (elemento (seleccion lista) lista) (random 8) '()))))

(define(mutacion-aux lista porcentaje resultado)
  (cond((null? lista)
        lista)
       (mutacion-aux (cdr lista) mutacion (+ resultado porcentaje (car lista))))) 


(define (seleccionar_azar_aux lista iteracion pos_por_mutar)
  (cond ((null lista?) '())
        ((equal? (car pos_por_mutar) iteracion) (cons (mutar_pos (car lista)) (seleccionar_azar_aux (cdr lista) (+ iteracion 1) (cdr pos_por_mutar))))
        (else
         (cons (car lista) (seleccionar_azar_aux (cdr lista) (+ iteracion 1) (pos_por_mutar))))))

(define (hacer_lista_random cant)
  (cond((zero? cant) '())
       (else
        (cons (random 1 4)(hacer_lista_random (- cant 1))))))

(define (mutar_pos elemento)
  (mutar_pos_aux (binario elemento) (random 0 5) 0))
(define (mutar_pos_aux num_binario pos iteracion)
  (cond ((zer

