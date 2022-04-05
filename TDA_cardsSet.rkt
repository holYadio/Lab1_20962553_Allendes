#lang racket
(provide (all-defined-out))
(require "TDA_element.rtk")
(require "TDA_card.rtk")
;; TDA cardsSet ;;
;Representacion:
; (list card)

;; Capa Constructora ;;
;Descripcion: Constructor de set de cartas
;Dom: list(lista de elementos) X int(numero de elementos en cada carta) X int(Maximo de cartas) X randomFn(Funcion de aleatorizacion)
;Rec: cardsSet
(define cardsSet
  (lambda (listaElementos cantElemento maxCartas rndFn)
    (list (car))))


; Funcion de utilidad ;
; Descripcion:Genera un numero aleatorio en torno al valor maximo
; Dom: entero
; Rec: entero

(define m 2147483647)
(define a 1103515245)
(define c 12345)
(define randomFn
  (lambda (xn)
    (modulo (+ (* a xn) c) m)
                 ))


