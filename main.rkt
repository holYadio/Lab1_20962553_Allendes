#lang racket
(require "TDA_element.rkt")
(require "TDA_card.rkt")
(require "TDA_cardsSet.rkt")
(require "TDA_player.rkt")
(require "TDA_tablero.rkt")
(require "TDA_game.rkt")

(define ListaElementos1 (list  1   2   3   4   5   6   7   8   9   10  11  12  13))
(define ListaElementos2 (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))
(define ListaElementos3 (list  1   2   2   2   2   2   2   8   9   10  11  12  13))


;;;;  ------------------------EJEMPLOS FUNCIONES TDA cardsSet------------------------  ;;;;
          ;---- Testing de la funcion Constructora cardsSet ----;
; Prerrequisistos: Ninguno
; Se crea dobbleSet1 que tiene 7 cartas de 4 elementos cada una.
(define dobbleSet1
  (cardsSet ListaElementos1 4 7 randomFn))

; Se crea dobbleSet2 que tiene 13 cartas de 4 elementos cada una.
(define dobbleSet2
  (cardsSet ListaElementos1 2 2 randomFn))

; Se crea dobbleSet3 que tiene 4 cartas de 3 elementos cada una.
(define dobbleSet3
  (cardsSet ListaElementos2 3 6 randomFn))




          ;---- Testing de la funcion dobble? ----;
; Prerrequisistos: constructor cardsSet
(define dobbleSet4
  '((2 7 10 13) (2 7 10 13) (1 5 6 7) (3 5 9 13) (4 5 10 12) (3 7 8 12) (3 6 10 11) (1 2 3 4) (1 11 12 13) (1 8 9 10) (4 6 8 13) (2 5 8 11) (2 6 9 12) (1 2 3 4 5 6 7 8 9 10 11 12 13)))
; Se ocupara dobbleSet4 que cuanta con una carta repetida por lo que el return debera ser un #f, haciendo referencia a que no es un conjunto valido.
(define dobbleSet6 (dobble? dobbleSet4))

(define dobbleSet5
  (cardsSet ListaElementos3 3 6 randomFn))
; Se empleara dobbleSet5 que genera un mazo con un conjunto de elementos que presenta elementos repetidos. 
(define dobbleSet7 (dobble? dobbleSet5))
; retornara un #f ya que no es valido el conjunto.

; Si aplicamos un dobble a dobbleSet2
(define dobbleSet8 (dobble? dobbleSet2))
; Retorna un #t dado que es un conjunto valido.




          ;---- Testing de la funcion numCards ----;
; Prerrequisistos: constructor cardsSet.
; Contamos el numero de cartas en el dobbleSet1 anteriormente definido.
(define dobbleSet10 (numCards dobbleSet1))

; Contamos el numero de cartas en el dobbleSet2 anteriormente definido.
(define dobbleSet11 (numCards dobbleSet2))

; Contamos el numero de cartas en el dobbleSet3 anteriormente definido.
(define dobbleSet12 (numCards dobbleSet3))




       ;--------- Testing de la funcion nthCard ---------;
; Prerrequisistos: constructor cardsSet
; Seleccionamos la primera carta del dobbleSet1 anteriormente definido.
(define dobbleSet13 (nthCard dobbleSet1 0))

; Seleccionamos la ultima carta del dobbleSet1 anteriormente definido.
(define dobbleSet14 (nthCard dobbleSet1 6))

; Seleccionamos la cuarta carta del dobbleSet3 anteriormente definido.
(define dobbleSet15 (nthCard dobbleSet3 3))




       ;--------- Testing de la funcion findTotalCards ---------;
; Prerrequisistos: constructor cardsSet, nthCard.
; Segun la primera carta del dobbleSet1 indica las cartas que se deben producir para que sea valido el conjunto.
(define dobbleSet16 (findTotalCards (nthCard dobbleSet1 0)))

; Segun la ultima carta del dobbleSet1 indica las cartas que se deben producir para que sea valido el conjunto.
(define dobbleSet17 (findTotalCards (nthCard dobbleSet1 6)))

; Segun la cuarta carta del dobbleSet3 indica las cartas que se deben producir para que sea valido el conjunto.
(define dobbleSet18 (findTotalCards (nthCard dobbleSet3 3)))




       ;--------- Testing de la funcion requiredElements ---------;
; Prerrequisistos: constructor cardsSet, nthCard
; Segun la primera carta del dobbleSet1 indica la cantidad de elementos para que sea valido el conjunto.
(define dobbleSet19 (requiredElements (nthCard dobbleSet1 0)))

; Segun la ultima carta del dobbleSet1 indica  la cantidad de elementos para que sea valido el conjunto.
(define dobbleSet20 (requiredElements (nthCard dobbleSet1 6)))

; Segun la cuarta carta del dobbleSet3 indica  la cantidad de elementos para que sea valido el conjunto.
(define dobbleSet21 (requiredElements (nthCard dobbleSet3 3)))




      ;--------- Testing de la funcion missingCards ---------;
; Prerrequisistos: constructor cardsSet, requiredElements, findTotalCards, numCards.
; Muestra el conjunto de cartas para que el dobbleSet1 sea valido.
(define dobbleSet22 (missingCards dobbleSet1))

; Muestra el conjunto de cartas para que el dobbleSet2 sea valido.
(define dobbleSet23 (missingCards dobbleSet2))

; Muestra el conjunto de cartas para que el dobbleSet3 sea valido.
(define dobbleSet24 (missingCards dobbleSet3))


         ;--------- Testing de la funcion cardsSet->string ---------;
; Prerrequisistos: missingCards
; transformamos nuestro dobbleSet1 para que ele usuario pueda visualizar el cardsSet.
(define dobbleSet25 (cardsSet->string  dobbleSet1))

; transformamos nuestro dobbleSet2 para que ele usuario pueda visualizar el cardsSet.
(define dobbleSet26 (cardsSet->string  dobbleSet2))

; transformamos nuestro dobbleSet3 para que ele usuario pueda visualizar el cardsSet.
(define dobbleSet27 (cardsSet->string  dobbleSet3))

(define dobbleSet28
  (cardsSet ListaElementos1 4 -1 randomFn))




;;;;  ------------------------EJEMPLOS FUNCIONES TDA game------------------------  ;;;;
     ;--------- Testing de la funcion constructora game ---------;
; Prerrequisistos: TDA cardsSet completo
; Creamos un juego de 4 jugadores con el dobbleSet 28, va a ser del modo stackMode y se ingresa la funcion de aleatorizacion.
(define game1
  (game 4 dobbleSet28 stackMode randomFn))

; Creamos un juego de 2 jugadores con el dobbleSet 28, va a ser del modo stackMode y se ingresa la funcion de aleatorizacion.
(define game2
  (game 2 dobbleSet28 stackMode randomFn))

; Creamos un juego de 4 jugadores con el dobbleSet 1, va a ser del modo stackMode y se ingresa la funcion de aleatorizacion.
(define game3
  (game 3 dobbleSet1 stackMode randomFn))




      ;--------- Testing de la funcion stackMode ---------;
; Prerrequisistos: constructor game
; Al aplicar el stackMode en el game se sacan 2 cartas de la pila de cartas y se muestran en el area de juego o tablero.
(define game4 (stackMode game1))
(define game5 (stackMode (stackMode game1)))
(define game6 (stackMode (stackMode (stackMode game1))))
; Esto se puede aplicar varias veces como se puede observar, esta funcion tiene mayor importancia junto a la funcion play.




     ;--------- Testing de la funcion register ---------;
; Prerrequisistos: TDA cardsSet completo, constructor game.
; Se registra un jugador en game2.
(define game7 (register "Javier" game2))
; (ERROR) se intentan registrar tres jugadores en el game2 cuando el limite es dos, asi que retorna el game con los primeros 2 ingresados dejando afuera al otro.
(define game8 (register "Javiera" (register "Pedro" (register "Juan" game2))))
; (ERROR)se registran 2 jugadores en game2 pero tienen el mismo nombre por lo que solo se registra una vez el jugador.
(define game9 (register "Luis" (register "Luis" game2)))




     ;--------- Testing de la funcion whoseTurnIsIt? ---------;
; Prerrequisistos: constructor game, register.
; Se consulta a quien le toca jugar dado el game7.
(define game10 (whoseTurnIsIt?  game7)) 
; Se consulta a quien le toca jugar dado el game8.
(define game11 (whoseTurnIsIt?  game8)) 
; Se consulta a quien le toca jugar dado el game9.
(define game12 (whoseTurnIsIt?  game9)) 
