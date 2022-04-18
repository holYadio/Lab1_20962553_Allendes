#lang racket
(provide (all-defined-out))
(require "TDA_element_20962553_AllendesFuentes.rkt")
(require "TDA_card_20962553_AllendesFuentes.rkt")
(require "TDA_cardsSet_20962553_AllendesFuentes.rkt")
(require "TDA_player_20962553_AllendesFuentes.rkt")
;; TDA tablero ;;
;Representacion:
; cartas en mesa X lista de jugadores X turnoJugador(int)


;; Capa Constructora ;;
;Descripcion: Constructor de un tablero.
;Dom: cartas en mesa(TDA) X lista de jugadores (list TDAs) X turnoJugador (int)
;Rec: tablero(TDA)
(define tablero
  (λ (cardsInTable listPlayer turnoPlayer)
    (list cardsInTable listPlayer turnoPlayer)
    )
  )


;; Capa Selector ;;
; Descripcion:  Selecciona el cardsSet del TDA.
; Dom: tablero(TDA)
; Rec: cardsSet(TDA)
(define getCardsSetInTablero
  (λ (tab)
  (car tab)
    )
  )


; Descripcion:  Selecciona la lista de jugadores del TDA.
; Dom: tablero(TDA)
; Rec: listaPlayer(list)
(define getListaPlayers
  (λ (tab)
  (cadr tab)
    )
  )


; Descripcion:  Selecciona el turno del TDA.
; Dom: tablero(TDA)
; Rec: turnoJugador(int)
(define getTurnoJugador
  (λ (tab)
  (caddr tab)
    )
  )


; Descripcion:  Selecciona los nombres de los jugadores del TDA.
; Dom: tablero(TDA)
; Rec:Lista de Nombres de los jugadores(list)
(define getListaNombrePlayers
  (λ (lista listaPlayers)
    (cond
      [(null? listaPlayers) lista]
      [else (getListaNombrePlayers (append (list (getNamePlayer (car listaPlayers))) lista) (cdr listaPlayers))]
      )
    )
  )


;; Otras operaciones ;;
;Descripcion: Constructor de un tablero vacio.
;Dom: null
;Rec: tablero(TDA)
(define empyTablero
  (list '() '() 1)
  )


;Descripcion: obtiene el n-simo jugador.
;Dom: posicion del jugador(int) X lista de jugadores(list)
;Rec: player(TDA)
(define getNplayer
  (λ (n listaPlayers)
    (cond
      [(eq? n 1) (car listaPlayers)]
      [else (getNplayer (- n 1) (cdr listaPlayers))])
    )
  )



