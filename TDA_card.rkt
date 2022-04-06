#lang racket
(provide (all-defined-out))
;; TDA cardsSet ;;
;Representacion:
; (list element1 element2 ... elementN)

;; Capa Constructora ;;
;Descripcion: Funcion Constructora de una carta
;Dom: list X int X randomFn
;Rec: card
(define card
  (lambda (lista int rndFn)
    (cond
      [(null? lista) null] 
      [()])))