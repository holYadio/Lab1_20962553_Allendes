#lang racket
(provide (all-defined-out))
(require "TDA_element.rkt")
(require "TDA_card.rkt")
;;; TDA cardsSet ;;;
;;Representacion;;
; (list card1 X card2 X ... X cardN X lista de Elementos)


;; Capa Constructora ;;
;Descripcion: Constructor de set de cartas.
;Dom: list(lista de elementos) X int(numero de elementos en cada carta) X int(Maximo de cartas) X randomFn(Funcion de aleatorizacion)
;Rec: cardsSet(TDA)
(define cardsSet
  (λ (listaElementos cantElemento maxCartas rndFn)
    (append (rndFn (createCardsSet listaElementos cantElemento maxCartas)
              '()
              1
              (length (createCardsSet listaElementos cantElemento maxCartas))
              (valRandom (createCardsSet listaElementos cantElemento maxCartas)
                         (random (length (createCardsSet listaElementos cantElemento maxCartas)))
                         )
              ) (list listaElementos))
    )
  )


;; Capa Pertenencia ;;
(define dobble?
  (λ (setCartas)
    (cond
      [(eq? 0 (remainder)) #T])))


;; Capa Selector ;;
; Descripcion:  Determina la lista de elementos del set de cartas.
; Dom: cardsSet(TDA)
; Rec: lista de elementos(TDA)
(define getListaElementos
  (λ (setCards)
    (car (reverse setCards))
    )
  )


; Descripcion:  Obtiene la n-ésima (nth) carta desde el conjunto de cartas partiendo desde 0 hasta (totalCartas-1).
; Dom: cardsSet(TDA) X Posicion de la carta(int)
; Rec: n-esima carta(TDA)
(define nthCard
  (λ (lista n) 
   (cond 
    [(eq? 0 n)(car lista)]
    [else (nthCard (cdr lista) (- n 1))])
    )
  )


;; Otras operaciones ;;
;Descripcion: crear lista de cartas.
;Dom: lista de elementos(list) X elementos por carta(int) X maximo de cartas(int) X contador(int) X contador(int) X contador(int)
;Rec: lista de cartas(list)
(define createCardsSet
  (λ (listaElementos elementosXCarta MCards)
    (cond
      [(<= MCards 0) (append (list (firstCard listaElementos elementosXCarta 1 '())) (nCards listaElementos elementosXCarta 1 1 '() '()) (n2Cards listaElementos elementosXCarta 1 1 1 '() '()))]
      [(> MCards 0) (acortarLista (createCardsSet listaElementos elementosXCarta -1) '() 1 MCards)]
      )
    )
  )


;Descripcion: crear n cartas.
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


;Descripcion: Crear el resto de cartas.
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


;Descripcion: Funcion agregar un elemento a una carta.
;Dom: lista de cartas(list) X carta(TDA)
;Rec: lista de cartas(list)
(define addCard
  (λ (listaCards card)
    (append listaCards card)
    )
  )


; Descripcion: valida si el numero esta en el rango valido de 0 y el largo de la lista.
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


; Descripcion: Randomiza set de cartas.
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


; Descripcion: Acorta la lista a un largo determinado.
; Dom: lista de cartas(list) X lista auxiliar(list) X contador(int) X largo deseado(int)
; Rec: lista de cartas(list)
(define acortarLista
  (λ (lista1 lista2 i x)
    (cond
      [(eq? x (- i 1)) (reverse lista2)]
      [else (cond
              [(eq? i (+ 1 (length lista1))) (reverse lista2)]
              [else (acortarLista lista1 (append (list (selElementoLista lista1 i)) lista2) (+ i 1) x)]
              )])
    )
  )


; Descripcion: Permite determinar la cantidad de cartas en el set.
; Dom: cardsSet(TDA)
; Rec: cantidad de cartas(int)
(define numCards
  (λ (cardsSet)
    (length cardsSet)
    )
  )


; Descripcion:  Determina la cantidad total de cartas que se deben producir para construir un conjunto válido.
; Dom: card(TDA)
; Rec: cantidad total de cartas que se deben producir para construir un conjunto válido(int)
(define findTotalCards
  (λ (card)
    (+ (* (- (length card) 1) (- (length card) 1)) (- (length card) 1) 1)
    )
  )


; Descripcion:  Determina la cantidad total de cartas que se deben producir para construir un conjunto válido.
; Dom: card(TDA)
; Rec: total de elementos necesarios para poder construir un conjunto válido(int)
(define requiredElements
  (λ (card)
    (+ (* (- (length card) 1) (- (length card) 1)) (- (length card) 1) 1)
    )
  )


; Descripcion:  Determina la cantidad total de cartas que se deben producir para construir un conjunto válido.
; Dom: card(TDA)
; Rec: cantidad de cartas(TDA)
(define missingCards
  (λ (setCards)
    (append (delCard setCards (length setCards) 0 (cardsSet (getListaElementos setCards) (length (nthCard setCards 1)) -1 randomFn)) (list (getListaElementos setCards)))
    )
  )


; Descripcion:  eliminar cartas de un set de cartas.
; Dom: cardsSet(TDA)
; Rec: cardsSet(TDA)
(define delCard
  (λ (setCards largoCards i complementoSetCards)
    (cond
      [(eq? largoCards i) complementoSetCards]
      [else (delCard setCards largoCards (+ i 1) (remove (nthCard setCards i) complementoSetCards))])
    )
  )


; Descripcion:  eliminar lista de elementos de un set de cartas.
; Dom: cardsSet(TDA)
; Rec: lista de cartas(list)
(define delListaElementos
  (λ (setCards)
    (reverse (cdr (reverse setCards)))
    )
  )


;Descripcion: Convierte un conjunto de cartas a una representación basada en strings.
;Dom: cardsSet(TDA)
;Rec: representacion en string (string)
;(define cardsSet->string
 ; (λ (cardsSet)))

;(define l1 (cardsSet (list  1   2   3   4   5   6   7   8   9   10  11  12  13) 4 1 randomFn))
(define l2 (cardsSet (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m") 4 4 randomFn))
;(missingCards  l2)
l2
(delListaElementos l2)