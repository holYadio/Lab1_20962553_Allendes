#lang racket
(provide (all-defined-out))
;; TDA cardsSet ;;
;Representacion:
; (list element1 element2 ... elementN)

;; Capa Constructora ;;
;Descripcion: Funcion Constructora de la primera carta
;Dom: list X int X randomFn
;Rec: card
;(define card
 ; (lambda (lista int rndFn)
  ;  (cond
   ;   [(null? lista) null] 
    ;  [()])))
(define firstCard
  (λ (listaElementos elementosXCarta i card)
    (cond
      [(eq? i elementosXCarta) (addElement card listaElementos i)]
      [else (firstCard listaElementos elementosXCarta (+ i 1) (addElement card listaElementos i))])))

(define nCard
  (λ (listaElementos elementosXCarta j k card)
    (cond
      [(eq? k (- elementosXCarta 1)) (append (list (car listaElementos)) (addElement card listaElementos (+ (* (- elementosXCarta 1) j) (+ k 1))))]
      [else (nCard listaElementos elementosXCarta j (+ k 1) (addElement card listaElementos (+ (* (- elementosXCarta 1) j) (+ k 1))))])))

(define n2Card
  (λ (listaElementos elementosXCarta i j k card)
    (cond
      [(eq? k (- elementosXCarta 1)) (append (list (selElementoLista listaElementos (+ i 1))) (addElement card listaElementos (+ (- elementosXCarta 1) 2 (* (- elementosXCarta 1) (- k 1)) (remainder (+ (* (- i 1) (- k 1)) (- j 1)) (- elementosXCarta 1)))))]
      [else (n2Card listaElementos elementosXCarta i j (+ k 1) (addElement card listaElementos (+ (- elementosXCarta 1) 2 (* (- elementosXCarta 1) (- k 1)) (remainder (+ (* (- i 1) (- k 1)) (- j 1)) (- elementosXCarta 1)))))])))

;; Otras Funciones ;;
(define addCard
  (λ (listaCards card)
    (append listaCards card)))

;Descripcion: Funcion agregar un elemento a una carta
;Dom: list X int
;Rec: element
(define addElement
  (λ (lista1 lista2 i)
    (append lista1 (list (selElementoLista lista2 i)))))


;Descripcion: Funcion para seleccionar el elemento n de la lista
;Dom: list X int
;Rec: element
(define selElementoLista
  (λ (lista n)
    (cond
      [(eq? n 1) (car lista)]
      [(> n 1) (selElementoLista (cdr lista) (- n 1))])))

;(firstCard (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m") 4 1 '())
;(nCard (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m") 4 1 1 '())
;(nCard (list  1   2   3   4   5   6   7   8   9   10  11  12  13) 4 1 1 '())
;(n2Card (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m") 4 1 1 1 '())
;(n2Card (list  1   2   3   4   5   6   7   8   9   10  11  12  13) 4 1 1 1 '())
