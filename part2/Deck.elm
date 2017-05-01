module Deck exposing (..)

-- imports --

-- type definitions --
type Color = Red | Black
type Suit = Heart | Spade | Diamond | Clover
type Face = 
  Ace | Two | Three | Four | Five | Six | Seven | 
  Eight | Nine | Ten | Jack | Queen | King
type Card = {face : Face, suit : Suit}
type alias Deck = List Card

