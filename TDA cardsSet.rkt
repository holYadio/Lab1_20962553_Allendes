#lang racket

;; TDA cardsSet ;;
;Representacion:
;(list elemento1 elemento2 elemento3 ... elemento n)

;; Capa Constructora ;;
;Descripcion: Constructor de set de cartas
;Dom: string X string X entero
;Rec: list



; Funcion de utilidad ;
; Descripcion:Genera un numero aleatorio en torno al valor maximo
; Dom: entero
; Rec: entero
(define randomInt (lambda (a) (random a)))

