;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tarea Corta 1
;; CCCE2019
;; Mariano Munioz Masis, Karla Rivera Sanchez, Daniel Prieto Sibaja
;; 2016121607, 2016100425, 2016072504
;;

;;NO VEO A NADIE HACIENDO CAMBIOS
;;NECESITAMOS AVANZAR 

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
;;Indice de Constante
;;#1 Largo_campo
;;#2 
;;Indice de Funciones
;;#1 Funcion CCCE
;;#2 hacer_equipos
;;#3 hacer_portero
;;#4 hacer_jugador_defensa
;;#5 hacer_jugador_central
;;#6 hacer_jugador_delantero
;;#7 hacer_bloque_defensivo
;;#8 hacer_bloque_central
;;#9 hacer_bloque_delantero
;;#10 obtener_dorsal
;;#11 obtener_habilidad
;;#12 obtener_fuerza
;;#13 obtener_velocidad 
;;#14 obtener_pos_inicial
;;#15 obtener_pos_final
;;#16 obtener_aptitud
;;#17 obtener_tipo
;;#18 suma_elementos_hasta
;;#19 suma_elementos
;;#20 funcion_random
;;#21 posicion_random
;;#22 inversa
;;#23 elemento_en_pos
;;#24 promedio_especial 
;;#25
;;#26
;;#27
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang racket
;;CONSTANTES
;;#1
(define largo_campo 1350)
;;#2
(define ancho_campo 650)
;;#3
(define numero_dorsal 2)
;;#4
(define equipo1 '())
;;#5
(define equipo2 '()) 



;;;;;;;;;;;;;;;;;;;;;;;;FUNCION PRINCIPAL;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;#1 
;;Funcion homologa al main
;;E: dos lista que representan la formacion de los jugadores
;;S: el inicio del juego
;;R: la suma de los elementos de la lista debe ser 
(define (CCCE2019 equipo_1 equipo_2)
  (cond ((or (> (suma_elementos equipo_1) 10) (> (suma_elementos equipo_2) 10))#f)
        (else
         (set! equipo1 (hacer_equipos equipo_1 numero_dorsal 1))
         (set! equipo2 (hacer_equipos equipo_2 numero_dorsal 2))
         (append (list 'E1 equipo1)(list 'E2 equipo2)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;#2 
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
;;#3
;;Funcion: Hacer Portero
;;E: -
;;S: la estructura de un jugador de tipo portero
(define(hacer_portero)
  (list '1 (numero-random) (numero-random) (numero-random) (posicion_random) (numero-random) '0 'portero)) 

;;#4
;;Funcion Hacer Delantero
;;E: numero de dorsal
;;S: Estructura de un delantero
(define(hacer_jugador_delantero dorsal)
  (list dorsal (numero-random) (numero-random) (numero-random) (posicion_random) (numero-random) '0 'delantero))
;;#5
;;Funcion Hacer Medio Campista
;;E: numero de dorsal
;;S: Estructura de un central
(define(hacer_jugador_central dorsal)
  (list dorsal (numero-random) (numero-random) (numero-random) (posicion_random) (numero-random) '0 'medio_campista))
;;#6
;;Funcion Hacer Defensa
;;E: numero de dorsal
;;S: Estructura de un Defensa
(define(hacer_jugador_defensa dorsal)
  (list dorsal (numero-random) (numero-random) (numero-random) (posicion_random) (numero-random) '0 'defensa))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;#7
;;Funcion Bloque Defensivo
;;E: cantidad de jugadores defensa por equipo
;;S: lista con los defensas por equipo
(define (hacer_bloque_defensivo cant_jugadores dorsal)
  (cond ((zero? cant_jugadores) '())
        (else
         (cons   (hacer_jugador_defensa dorsal)(hacer_bloque_defensivo (- cant_jugadores 1) (+ dorsal 1))))))
;;#8
;;Funcion Bloque Central
;;E: cantidad de jugadores centrales por equipo
;;S: lista con los centralesf por equipo
(define (hacer_bloque_central cant_jugadores dorsal)
  (cond ((zero? cant_jugadores) '())
        (else
         (cons  (hacer_jugador_central dorsal) (hacer_bloque_central (- cant_jugadores 1)(+ dorsal 1))))))
;;#9
;;Funcion Bloque Delantero
;;E: cantidad de jugadores delanteros por equipo
;;S: lista con los delanteros por equipo
(define (hacer_bloque_delantero cant_jugadores dorsal)
  (cond ((zero? cant_jugadores) '())
        (else
         (cons  (hacer_jugador_delantero dorsal) (hacer_bloque_delantero (- cant_jugadores 1) (+ dorsal 1))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;Getters y Setters;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;#10
(define (obtener_dorsal lista) (elemento_en_posicion lista 0))
;;#11
(define (obtener_habilidad lista) (elemento_en_posicion lista 1))
;;#12
(define (obtener_fuerza lista) (elemento_en_posicion lista 2))
;;#13
(define (obtener_velocidad lista) (elemento_en_posicion lista 3))
;;#14
(define (obtener_posicion_inicial lista) (elemento_en_posicion lista 4))
;;#15
(define (obtener_posicion_final lista) (elemento_en_posicion lista 5))
;;#16
(define (obtener_aptitud lista) (elemento_en_posicion lista 6))
;;#17
(define (obtener_tipo lista) (elemento_en_posicion lista 7))



;;;;;;;;;;;;;;;;;;;;;FUNCIONES AUXILIARES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;#18
;;Funcion Suma elementos hasta
;;E: una lista y un indice de parada
;;S: un entero resultado de la suma de los valores de los indices
;;R: valores son enteros 
(define (suma_elementos_hasta lista indice)
  (cond ((zero? indice) 0)
        (else
         ( + (suma_elementos_hasta_aux (car lista)) (suma_elementos_hasta (cdr lista) (- indice 1)))))) 

(define (suma_elementos_hasta_aux cant)
  (cond ((zero? cant) 0)
        (else (+ 1 (suma_elementos_hasta_aux (- cant 1))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;/;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;#19
;; Funcion Suma Elementos
;;E: una lista
;;S: entero con la suma de los valores de la lista
;;R: lista de enteros
(define (suma_elementos lista)
  (cond ((null? lista) 0)
        (else
         ( + (suma_elementos_aux (car lista)) (suma_elementos (cdr lista)))))) 

(define (suma_elementos_aux cant)
  (cond ((zero? cant) 0)
        (else (+ 1 (suma_elementos_aux (- cant 1))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;/;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;#20
;;Funcion Aleatoria
;;E: -
;;S: Un numero aleatorio
(define(numero-random)
  (random 0 20))

;;#21
;;Funcion que devuelve una posicion
;;E: -
;;S: Posicion random
(define (posicion_random)
  (list (random 0 1080)(random 0 680)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;#22
;;Funcion Inversa de una Lista
(define(inversa lista)
  (cond ((null? lista) '())
        (else (append (inversa (cdr lista)) (list(car lista))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;#23
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
;;#24
;;Funcion Promedio Especial
;;E: dos listas de jugadores
;;S: una nueva lista recombinada
;;R: - 
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (nueva_pos listaA listaB)
  (cond ((null? listaA) '())
        (else
         (cons (quotient (+(car listaA)(car listaB))2) (nueva_pos (cdr listaA) (cdr listaB))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (seleccionar_azar lista)
  (seleccionar_azar_aux lista 0 (hacer_lista_random (random 0 4))))

(define (seleccionar_azar_aux lista iteracion pos_por_mutar)
  (cond ((null? lista) '())
        ((equal? (car pos_por_mutar) iteracion) (cons (mutar_pos (car lista)) (seleccionar_azar_aux (cdr lista) (+ iteracion 1) (cdr pos_por_mutar))))
        (else
         (cons (car lista) (seleccionar_azar_aux (cdr lista) (+ iteracion 1) (pos_por_mutar))))))

(define (hacer_lista_random cant)
  (cond((zero? cant) '())
       (else
        (cons (random 1 4)(hacer_lista_random (- cant 1))))))

(define (mutar_pos elemento) 0)
 ;; (mutar_pos_aux (binario elemento) (random 0 5) 0))
;;(define (mutar_pos_aux num_binario pos iteracion)
  ;;(cond ((zer



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;ALGORITMO GENETICO;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Algoritmo Genético, hagamos pruebas

;;Funcion que suma todos los elementos de una lista
(define(sum lista)
  (cond((null? lista)
        0)
       (else
        (+ (car lista) (sum (cdr lista))))))
;; Funcion que devuelve el promedio de una lista
(define(promedio lista)
  (cond((null? lista)
        0)
       (else
        (quotient (sum lista)  (length lista)))))
;;Funcion que devuelve el cuadrado de un numero
(define(cuadrado num)
  (* num num))
;;Funcion que devuelve la hipotenusa de dos medidas
(define(hipotenusa a b)
  (cond((zero? a)
        (cuadrado b))
       ((zero? b)
        (cuadrado a))
       (else
        (+ (cuadrado a) (cuadrado b)))))
;;Funcion que devuelve el elemento de una lista en una posicion dada.
(define(elemento index lista)
  (cond((null? lista)
        '())
       ((equal? index 1)
        (car lista))
       (else
        (elemento (- index 1) (cdr lista)))))
;;Funcion que devuelve el elemento mayor de una lista
(define(mayor ele lista)
  (cond((null? lista)
        ele)
       ((< ele (car lista))
        (mayor (car lista) (cdr lista)))
       (else
        (mayor ele (cdr lista)))))
;;Funcion que aplica una funcion dada a una lista
(define(aplicar fun lista)
  (cond((null? lista)
        '())
       (else
        (cons (fun (car lista)) (aplicar fun (cdr lista))))))
;;Funcion que reproduce dos padres
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

(define(aptitud-aux lista)
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
       (else
        (mutacion-aux (cdr lista) porcentaje (+ resultado porcentaje (car lista))))))

(define(genetico lista)
  (cond((null? lista)
        0)
       (else
        (append(mutacion (seleccion (reproduccion (car lista) (cdr lista)))) (genetico (cdr lista))))))



