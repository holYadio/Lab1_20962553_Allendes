#lang racket
(provide (all-defined-out))
(require "TDA_element.rkt")
(require "TDA_card.rkt")
(require "TDA_cardsSet.rkt")
(require "TDA_player.rkt")
(require "TDA_tablero.rkt")
;;; TDA cardsSet ;;;
;;Representacion;;
; nJugadores X cardsSet X mode X rdnFn X tablero


;; Capa Constructora ;;
;Descripcion: Constructor de un juego.
;Dom: numPlayers(int) X cardsSet(TDA) X mode (fn) X rndFn (fn)
;Rec: game(TDA)
(define game
  (λ (nPlayers cardsSet mode rndFn)
    (createGame nPlayers cardsSet mode rndFn empyTablero)
    )
  )

;Descripcion: Constructor auxiliar del juego.
;Dom: numPlayers(int) X cardsSet(TDA) X mode (fn) X rndFn (fn)
;Rec: game(TDA)
(define createGame
  (λ (nPlayers cardsSet mode rndFn tab)
    (list nPlayers cardsSet mode rndFn tab)
    )
  )



;; Capa Pertenencia ;;
; Descripcion:  Verifica si el usuario ya esta registrado en el juego.
; Dom: nombre(str) X game(TDA)
; Rec: booleano(bool)
(define existeUsuario?
  (λ (usuario gm)
    (cond
      [(member usuario (getListaNombrePlayers '() (getListaPlayers (getTablero gm)))) #t]
      [else #f])
    )
  )


; Descripcion:  Verifica si se puede registrar un nuevo usuario en el juego.
; Dom: nombre(str) X game(TDA)
; Rec: booleano(bool)
(define ExcedeCantPlayers?
  (λ (gm)
    (cond 
      [(< (length (getListaPlayers (getTablero gm))) (getNPlayers gm)) #f]
      [else #t])
    )
  )


;; Capa Selector ;;
; Descripcion:  Selecciona el numero maximo de jugadores del TDA game.
; Dom: game(TDA)
; Rec: Numero de jugadores(int)
(define getNPlayers
  (λ (gm)
    (car gm)
    )
  )


; Descripcion:  Selecciona el cardsSet del TDA game.
; Dom: game(TDA)
; Rec: cardsSet(TDA)
(define getCardsSet
  (λ (gm)
    (cadr gm)
    )
  )


; Descripcion:  Selecciona el cardsSet del TDA game.
; Dom: game(TDA)
; Rec: modo(Fn)
(define getMode
  (λ (gm)
    (caddr gm)
    )
  )


; Descripcion:  Selecciona el cardsSet del TDA game.
; Dom: game(TDA)
; Rec: funcion de aleatorizacion(Fn)
(define getRndFn
  (λ (gm)
    (cadddr gm)
    )
  )


; Descripcion:  Selecciona el tablero del TDA game.
; Dom: game(TDA)
; Rec: tablero(TDA)
(define getTablero
  (λ (gm)
    (cadddr (cdr gm))
    )
  )


;; Capa modificador ;;
; Descripcion:  Modifica el cardsSet del TDA game.
; Dom: game(TDA) X cardsSet(TDA)
; Rec: game(TDA)
(define setCardsSet
  (λ (newSetCards gm)
    (createGame (getNPlayers gm)
                newSetCards
                (getMode gm)
                (getRndFn gm)
                (getTablero gm))
    )
  )


; Descripcion:  Agrega un jugador al juego.
; Dom: nombreUsuario(str) X game(TDA)
; Rec: game(TDA)
(define register
  (λ (nombreUsuario gm)
    (cond
      [(ExcedeCantPlayers? gm) gm]
      [else (cond
              [(eq? (existeUsuario? nombreUsuario gm) #t) gm]
              [else (createGame (getNPlayers gm)
                                (getCardsSet gm)
                                (getMode gm)
                                (getRndFn gm)
                                (tablero (getCardsSetInTablero (getTablero gm))
                                         (append (getListaPlayers (getTablero gm))
                                                 (list (player nombreUsuario
                                                               empyCardsSet
                                                               (+ (length (getListaPlayers (getTablero gm))) 1))))
                                         (getTurnoJugador (getTablero gm))))])])
    )
  )


;; Otras operaciones ;;
; Descripcion:  Permite retirar y voltear las dos cartas superiores del stack de cartas en el juego y las dispone en el área de juego.
; Dom: game(TDA)
; Rec: game(TDA)
(define stackMode
  (λ (gm)
    (createGame (getNPlayers gm)
          (delCard (getCardsSet gm) (- (length (getCardsSet gm)) 1) (- (length (getCardsSet gm)) 3) (getCardsSet gm))
          (getMode gm)
          (getRndFn gm)
          (tablero (append (list (nthCard (getCardsSet gm) (- (length (getCardsSet gm)) 3))) (append (list (nthCard (getCardsSet gm) (- (length (getCardsSet gm)) 2))) (getCardsSetInTablero (getTablero gm))))
                   (getListaPlayers (getTablero gm))
                   (getTurnoJugador (getTablero gm))))
    )
  )


; Descripcion: Retorna el usuario a quién le corresponde jugar en el turno.
; Dom: game(TDA)
; Rec: Usuario a quien le corresponde jugar(str)
(define whoseTurnIsIt?
 (λ (gm)
    (string-append "Le corresponde jugar al jugador " (~a (getTurnoJugador (getTablero gm))) ": " (getNamePlayer (getNplayer (getTurnoJugador (getTablero gm)) (getListaPlayers (getTablero gm)))) "\n")))







;(define l1 '((3 7 8 12) (1 11 12 13) (2 6 9 12) (2 5 8 11) (1 2 3 4) (4 5 10 12) (4 7 9 11) (1 5 6 7) (4 6 8 13) (2 7 10 13) (3 5 9 13) (1 8 9 10) (3 6 10 11) (1 2 3 4 5 6 7 8 9 10 11 12 13)))
;(define numPlayers 4)
;(define numElementsPerCard 4)
;(define maxCards 10)
;(define elements1 (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"))
;(define elements2 (list  1   2   3   4   5   6   7   8   9   10  11  12  13))


;(define dobbleSet0 (cardsSet elements2 numElementsPerCard -1 randomFn))
;(define dobbleSet1 (cardsSet elements1 numElementsPerCard -1 randomFn))
;(missingCards  l2)
;l2
;(delListaElementos l2)
;(display (cardsSet->string l2))
;dobbleSet0
;dobbleSet1
;(define game1(createGame 4 dobbleSet0 "stackMode" "randomFn" empyTablero))
;(define game2 (register "uwu" (register "awa" game1)))
;(display (whoseTurnIsIt? game2))
;(setCardsSet dobbleSet1 game1)