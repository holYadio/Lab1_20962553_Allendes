#lang racket
(provide (all-defined-out))
(require "TDA_element.rkt")
(require "TDA_card.rkt")
;;; TDA cardsSet ;;;
;;Representacion;;
; (list card1 card2 ... cardN)


;; Capa Constructora ;;
;Descripcion: Constructor de set de cartas
;Dom: list(lista de elementos) X int(numero de elementos en cada carta) X int(Maximo de cartas) X randomFn(Funcion de aleatorizacion)
;Rec: cardsSet(TDA)
(define cardsSet
  (λ (listaElementos cantElemento maxCartas rndFn)
    (rndFn (createCardsSet listaElementos cantElemento maxCartas)
              '()
              1
              (length (createCardsSet listaElementos cantElemento maxCartas))
              (valRandom (createCardsSet listaElementos cantElemento maxCartas)
                         (random (length (createCardsSet listaElementos cantElemento maxCartas)))
                         )
              )
    )
  )


;; Capa Pertenencia ;;
(define dobble?
  (λ (setCartas)
    (cond
      [(eq? 0 (remainder)) #T])))


;; Funcion de utilidad ;;
;Descripcion: crear lista de cartas
;Dom: lista de elementos(list) X elementos por carta(int) X maximo de cartas(int) X contador(int) X contador(int) X contador(int)
;Rec: lista de cartas(list)
(define createCardsSet
  (λ (listaElementos elementosXCarta MCards)
    (cond
      [(< MCards 0) (append (list (firstCard listaElementos elementosXCarta 1 '())) (nCards listaElementos elementosXCarta 1 1 '() '()) (n2Cards listaElementos elementosXCarta 1 1 1 '() '()))]
      [(> MCards 0) (acortarLista (createCardsSet listaElementos elementosXCarta -1) '() 1 MCards)]
      )
    )
  )


;Descripcion: crear n cartas
;Dom: lista de elementos(list) X elementos por carta(int) X contador(int) X contador(int) X lista de cartas(list) X carta(TDA card)
;Rec: lista de cartas(list)
(define nCards
  (λ (listaElementos elementosXCarta j k listacard card)
    (cond
      [(eq? j elementosXCarta) listacard]
      [else (append (list (nCard listaElementos elementosXCarta j k card)) (nCards listaElementos elementosXCarta (+ j 1) k listacard card))]
      )
    )
  )


;Descripcion: Crear el resto de cartas
;Dom: lista de elementos(list) X elementos por carta(int) X contador(int) X contador(int) X contador(int) X lista de cartas(list) X carta(TDA card)
;Rec: lista de cartas(list)
(define n2Cards
  (λ (listaElementos elementosXCarta i j k listacard card)
    (cond
      [(eq? i (- elementosXCarta 1)) (cond
               [(eq? j elementosXCarta) listacard]
               [else (append (list (n2Card listaElementos elementosXCarta i j k card)) (n2Cards listaElementos elementosXCarta i (+ j 1) 1 listacard card))])]
      [else (cond
              [(eq? j (- elementosXCarta 1)) (append (list (n2Card listaElementos elementosXCarta i j k card)) (n2Cards listaElementos elementosXCarta (+ i 1) 1 1 listacard card))]
              [else (append (list (n2Card listaElementos elementosXCarta i j k card)) (n2Cards listaElementos elementosXCarta i (+ j 1) 1 listacard card))]
              )
            ]
      )
    )
  )


;Descripcion: Funcion agregar un elemento a una carta
;Dom: lista de cartas(list) X carta(TDA)
;Rec: lista de cartas(list)
(define addCard
  (λ (listaCards card)
    (append listaCards card)
    )
  )


; Descripcion: valida si el numero esta en el rango valido de 0 y el largo de la lista
; Dom: lista de cartas(list) X numero random(int)
; Rec: numero random valido(int)
(define valRandom
  (λ (largoL x)
    (cond
      [(eq? x largoL) (- x 1)]
      [(eq? x 0) (+ x 1)]
      [else x]
      )
    )
  )


; Descripcion: Randomiza set de cartas
; Dom: lista de cartas(list) X lista auxiliar(list) X contador(int) X largo de lista de cartas(int) X numero random entre 0 y el largo de la lista(int)
; Rec: lista de cartas(list)
(define randomFn
  (λ (lista1 lista2 i largoL1 x)
    (cond
      [(eq? i (+ largoL1 1)) lista2]
      [else (randomFn (remove (selElementoLista lista1 x) lista1) (append (list (selElementoLista lista1 x)) lista2) (+ i 1) largoL1 (valRandom largoL1 (random (length lista1))))]
      )
    )
  )


; Descripcion: Acorta la lista a un largo determinado
; Dom: lista de cartas(list) X lista auxiliar(list) X contador(int) X largo deseado(int)
; Rec: lista de cartas(list)
(define acortarLista
  (λ (lista1 lista2 i x)
    (cond
      [(eq? x (- i 1)) (reverse lista2)]
      [else (cond
              [(eq? i (+ 1 (length lista1))) (reverse lista2)]
              [else (acortarLista lista1 (append (list (selElementoLista lista1 i)) lista2) (+ i 1) x)]
              )]
      )
    )
  )


(define numCards
  (λ (cardsSet)))


(define l1 (cardsSet (list  1   2   3   4   5   6   7   8   9   10  11  12  13) 4 12 randomFn))
(define l2 (acortarLista l1 '() 1 5))







