#lang racket
(provide (all-defined-out))
(require "TDA_element.rtk")
;; TDA cardsSet ;;
;Representacion:
; lista de elementos X numero de elementos en cada carta X Maximo de cartas X Funcion de aleatorizacion

;; Capa Constructora ;;
;Descripcion: Constructor de set de cartas
;Dom: string X int X int
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

