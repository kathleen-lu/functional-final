module Deck exposing (..)

-- imports --

-- type definitions --
type Color = Red | Black
type Suit = Heart | Spade | Diamond | Clover
type Face = 
  Ace | Two | Three | Four | Five | Six | Seven | 
  Eight | Nine | Ten | Jack | Queen | King
type alias Card = { face : Face, suit : Suit }
type alias Deck = List Card

-- variables --

startingDeck = [{face = Ace, suit = Heart}, {face = Ace, suit = Spade},
              {face = Ace, suit = Diamond}, {face = Ace, suit = Clover},
              {face = Two, suit = Heart}, {face = Two, suit = Spade},
              {face = Two, suit = Diamond}, {face = Two, suit = Clover},
              {face = Three, suit = Heart}, {face = Three, suit = Spade},
              {face = Three, suit = Diamond}, {face = Three, suit = Clover},
              {face = Four, suit = Heart}, {face = Four, suit = Spade},
              {face = Four, suit = Diamond}, {face = Four, suit = Clover},
              {face = Five, suit = Heart}, {face = Five, suit = Spade},
              {face = Five, suit = Diamond}, {face = Five, suit = Clover},
              {face = Six, suit = Heart}, {face = Six, suit = Spade},
              {face = Six, suit = Diamond}, {face = Six, suit = Clover},
              {face = Seven, suit = Heart}, {face = Seven, suit = Spade},
              {face = Seven, suit = Diamond}, {face = Seven, suit = Clover},
              {face = Eight, suit = Heart}, {face = Eight, suit = Spade},
              {face = Eight, suit = Diamond}, {face = Eight, suit = Clover},
              {face = Nine, suit = Heart}, {face = Nine, suit = Spade},
              {face = Nine, suit = Diamond}, {face = Nine, suit = Clover},
              {face = Ten, suit = Heart}, {face = Ten, suit = Spade},
              {face = Ten, suit = Diamond}, {face = Ten, suit = Clover},
              {face = Jack, suit = Heart}, {face = Jack, suit = Spade},
              {face = Jack, suit = Diamond}, {face = Jack, suit = Clover},
              {face = Queen, suit = Heart}, {face = Queen, suit = Spade},
              {face = Queen, suit = Diamond}, {face = Queen, suit = Clover},
              {face = King, suit = Heart}, {face = King, suit = Spade},
              {face = King, suit = Diamond}, {face = King, suit = Clover}]

