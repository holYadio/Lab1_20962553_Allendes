#lang racket
(require "TDA_element.rkt")
(require "TDA_card.rkt")
(require "TDA_cardsSet.rkt")
(require "TDA_player.rkt")
(provide (all-defined-out))
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


(define getNplayer
  (λ (n listaPlayers)
    (cond
      [(eq? n 1) (car listaPlayers)]
      [else (getNplayer (- n 1) (cdr listaPlayers))])))




;(define l1 '((3 7 8 12) (1 11 12 13) (2 6 9 12) (2 5 8 11) (1 2 3 4) (4 5 10 12) (4 7 9 11) (1 5 6 7) (4 6 8 13) (2 7 10 13) (3 5 9 13) (1 8 9 10) (3 6 10 11) (1 2 3 4 5 6 7 8 9 10 11 12 13)))
;(getNplayer 1 (list (player "uwu" l1 1) (player "owo" l1 2) (player "ewe" l1 3)))




