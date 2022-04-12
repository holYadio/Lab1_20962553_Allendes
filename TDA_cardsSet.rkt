#lang racket
(provide (all-defined-out))
(require "TDA_element.rkt")
(require "TDA_card.rkt")
;; TDA cardsSet ;;
;Representacion:
; (list card)



; orden del plano es cantElemento - 1
;; Capa Constructora ;;
;Descripcion: Constructor de set de cartas
;Dom: list(lista de elementos) X int(numero de elementos en cada carta) X int(Maximo de cartas) X randomFn(Funcion de aleatorizacion)
;Rec: cardsSet
;(define cardsSet
 ; (λ (listaElementos cantElemento maxCartas rndFn)
  ;  (cond
   ;   [(eq? i 1)()]
    ;  [])))

(define createCardsSet
  (λ (listaElementos elementosXCarta MCards i j k)
    (cond
      [(< MCards 0) (append (list (firstCard listaElementos elementosXCarta i '())) (nCards listaElementos elementosXCarta j k '() '()) (n2Cards listaElementos elementosXCarta i j k '() '()))])))




(define nCards
  (λ (listaElementos elementosXCarta j k listacard card)
    (cond
      [(eq? j elementosXCarta) listacard]
      [else (append (list (nCard listaElementos elementosXCarta j k card)) (nCards listaElementos elementosXCarta (+ j 1) k listacard card))])))

(define n2Cards
  (λ (listaElementos elementosXCarta i j k listacard card)
    (cond
      [(eq? i (- elementosXCarta 1)) (cond
               [(eq? j elementosXCarta) listacard]
               [else (append (list (n2Card listaElementos elementosXCarta i j k card)) (n2Cards listaElementos elementosXCarta i (+ j 1) 1 listacard card))])]
      [else (cond
              [(eq? j (- elementosXCarta 1)) (append (list (n2Card listaElementos elementosXCarta i j k card)) (n2Cards listaElementos elementosXCarta (+ i 1) 1 1 listacard card))]
              [else (append (list (n2Card listaElementos elementosXCarta i j k card)) (n2Cards listaElementos elementosXCarta i (+ j 1) 1 listacard card))])])))

; Funcion de utilidad ;
; Descripcion:Genera un numero aleatorio en torno al valor maximo
; Dom: entero
; Rec: entero


(define randomInt
  (λ (lista)
    (random
     (length lista))))

(define randomFn
  (λ (lista1 lista2 x)
    (cond
      [(null? lista1) lista2]
      [else (randomFn (remove x lista1) (append (list (selElementoLista lista1 x)) (lista2)))])))

;(nCards (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m") 4 1 1 '() '())
;(nCards (list  1   2   3   4   5   6   7   8   9   10  11  12  13) 4 1 1 '() '())
;(n2Cards (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m") 4 1 1 1 '() '())
;(n2Cards (list  1   2   3   4   5   6   7   8   9   10  11  12  13) 4 1 1 1 '() '())
(define a1 (createCardsSet (list  1   2   3   4   5   6   7   8   9   10  11  12  13) 4 -1 1 1 1))
(randomFn (createCardsSet (list  1   2   3   4   5   6   7   8   9   10  11  12  13) 4 -1 1 1 1) '() (randomInt(createCardsSet (list  1   2   3   4   5   6   7   8   9   10  11  12  13) 4 -1 1 1 1)))
