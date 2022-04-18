#lang racket
(provide (all-defined-out))
;; TDA player ;;
;Representacion:
; user X cardsSet X orden


;; Capa Constructora ;;
;Descripcion: Constructor de un jugador.
;Dom: NombreJugador(str) X cardsSet(TDA) X nJugador (int)
;Rec: player(TDA)
(define player
  (λ (namePlayer cardsSetPlayer nPlayer)
  (list namePlayer cardsSetPlayer nPlayer)
    )
  )


;; Capa Selector ;;
; Descripcion:  Selecciona el nombre del jugador del TDA.
; Dom: player(TDA)
; Rec: NombreJugador(str)
(define getNamePlayer
  (λ (pl)
  (car pl)
    )
  )


; Descripcion:  Selecciona el cardsSet del jugador del TDA.
; Dom: player(TDA)
; Rec: cardsSet(TDA)
(define getcardsSetPlayer
  (λ (pl)
  (cadr pl)

    )
  )


; Descripcion:  Selecciona el numero del jugador del TDA.
; Dom: player(TDA)
; Rec: NumJugador(int)
(define getNumPlayer
  (λ (pl)
  (caddr pl)
    )
  )
