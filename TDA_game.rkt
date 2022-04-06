#lang racket
(require "TDA_element.rkt")
(require "TDA_card.rkt")
(require "TDA_cardsSet.rkt") 
(define game
  (lambda (numPlayers cardsSet Mode rndFn)
    (list numPlayers cardsSet Mode rndFn)))