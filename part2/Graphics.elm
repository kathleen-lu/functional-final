module Graphics exposing(..)

-- imports
import Deck exposing(Suit(..), Face(..), Card, Deck)
import Color
import Collage
import Element exposing(Element)
import Text exposing(fromString)
import Html.Attributes as Attribute exposing(style, shape, coords)
import Html exposing (..)

-- constants
red = Collage.filled Color.red
black = Collage.filled Color.black
s = 4
h = 150
w = 100
emptyCollage = Collage.filled Color.white (Collage.circle 1)
cbg = Collage.outlined (Collage.solid Color.gray) (Collage.rect w h)
c = Color.rgb 96 134 229

-- functions
renderHand : List Card -> Collage.Form
renderHand cards = 
  case cards of
    [] -> emptyCollage -- lol
    fst::rest -> 
      Collage.group [ renderFaceUp fst,
                      Collage.move (w*1.1,0) <| renderHand rest]

renderFacedownHand : List Card -> Collage.Form
renderFacedownHand cards = 
  case cards of
    [] -> emptyCollage
    fst::rest -> 
      Collage.group [ renderFaceDown,
                      Collage.move (w*1.1,0) <| renderFacedownHand rest]

-- TODO iteration 2 maybe make the rendering more specific to the deck 
-- rather than just a facedown card
renderDeck : Deck -> Collage.Form
renderDeck deck = 
  renderFaceDown

-- creates a face up card given a card (face and suit)
renderFaceUp : Card -> Collage.Form
renderFaceUp card =
  let face = card.face in 
  let suit = card.suit in
  let tc = Collage.scale 0.6 <| renderTopCorner face suit in 
  let bc = Collage.scale 0.6 <| renderBottomCorner face suit in
  let ct = Collage.scale 0.6 <| renderCenterImg suit in
    Collage.group [ cbg,
                    Collage.move (-w/2.5,h/2.5) tc,
                    Collage.move (w/2.5,-h/2.5) bc,
                    ct
                  ] 

-- creates a generic face down card whose values are unknown
renderFaceDown : Collage.Form
renderFaceDown =
  let innerBorder = Collage.filled c (Collage.rect (w/1.13) (h/1.1)) in
  let border = Collage.outlined (Collage.solid c) (Collage.rect w h) in
    Collage.group [border, innerBorder]

-- helpers for rendering a face up card
renderCenterImg : Suit -> Collage.Form
renderCenterImg suit = 
  Collage.scale 2.6 <| renderSuit suit

renderTopCorner : Face -> Suit -> Collage.Form
renderTopCorner face suit = 
  let dist = 8 in 
  let si = Collage.moveY -dist <| renderSuit suit in
  let letter = Collage.scale 1.4 <| Collage.toForm <| Element.centered <| fromString <| faceToString face in 
  let fi = Collage.moveY dist letter in
    Collage.group [si, fi]

renderBottomCorner : Face -> Suit -> Collage.Form
renderBottomCorner face suit = 
  Collage.rotate (degrees 180) <| renderTopCorner face suit

renderSuit : Suit -> Collage.Form 
renderSuit suit = 
  case suit of
    Heart -> renderHeart
    Clover -> renderClover
    Spade -> renderSpade
    Diamond -> renderDiamond

renderHeart : Collage.Form
renderHeart = 
  Collage.move (0,-s*0.8) <| Collage.group [ Collage.move (s, 0) <| red (Collage.circle s),
                  Collage.move (-s, 0) <| red (Collage.circle s),
                  Collage.move (0,-(s+0.4)) <| Collage.rotate (degrees 150) <| red (Collage.ngon 3 (s+5.95)) 
                ]

renderClover : Collage.Form
renderClover = 
  let m = s*1.2 in
  Collage.move (0,-s*0.8) <| Collage.group [ Collage.move (m,0) <| black (Collage.circle s),
                  Collage.move (-m,0) <| black (Collage.circle s),
                  Collage.move (0,m) <| black (Collage.circle s),
                  Collage.move (0,0) <| black (Collage.square m),
                  Collage.move (0,-m) <| Collage.rotate (degrees -150) <| black (Collage.ngon 3 (s+1))
                ]

renderSpade : Collage.Form
renderSpade =
  let m = s*1.2 in 
  Collage.move (0,-s*0.9) <| Collage.group [ Collage.move (s, -s) <| black (Collage.circle s),
                  Collage.move (-s, -s) <| black (Collage.circle s),
                  Collage.move (0,0.8) <| Collage.rotate (degrees -150) <| black (Collage.ngon 3 (s+5.6)),
                  Collage.move (0,-m*2) <| Collage.rotate (degrees -150) <| black (Collage.ngon 3 (s+1)) 
                ]

renderDiamond : Collage.Form
renderDiamond =
  let w = 12 in 
    Collage.move (-s*0.8,-s*0.8) <| Collage.group [ Collage.move (s,0) <| Collage.rotate (degrees 45) <| red (Collage.square w) ]

-- helpers for converting the face into a visualizable string
faceToString : Face -> String 
faceToString face = 
  case face of
    Ace -> "A"
    Two -> "2"
    Three -> "3"
    Four -> "4"
    Five -> "5"
    Six -> "6"
    Seven -> "7"
    Eight -> "8"
    Nine -> "9"
    Ten -> "10"
    Jack -> "J"
    Queen -> "Q"
    King -> "K"

cardToString : Card -> String
cardToString card = 
  case card.suit of
    Heart -> faceToString card.face ++ " of Hearts"
    Clover -> faceToString card.face ++ " of Clubs"
    Diamond -> faceToString card.face ++ " of Diamonds"
    Spade -> faceToString card.face ++ " of Spades"

-- TODO iteration 2 : add in the number of symbols according to card
faceToInt : Face -> Int 
faceToInt face = 
  case face of
    Ace -> 1
    Two -> 2
    Three -> 3
    Four -> 4
    Five -> 5
    Six -> 6
    Seven -> 7
    Eight -> 8
    Nine -> 9
    Ten -> 10
    Jack -> 11
    Queen -> 12
    King -> 13

