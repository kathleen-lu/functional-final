module Mechanics exposing (..)

-- imports --
import Deck as D
import Random

-- type definitions --

type alias Hand = List D.Card 
type alias Score = { pairs: List D.Face, points: Int }
type alias Player = { name : String, id : Int, hand : Hand, score : Score }
type alias Game = { players : List Player, deck : D.Deck, current : Player }

remove : D.Face -> Hand -> Maybe Hand 
remove f h = 
-- if remove returns Nothing --> Go Fish!
  (case h of 
    c::cs ->  if f == c.face then 
                Just cs 
              else 
                (case remove f cs of 
                  Nothing -> Nothing 
                  Just hand -> Just (c::hand))
    [] -> Nothing)

updateScore : (D.Face, Score) -> Score
updateScore (f, s) = 
  { s | pairs = f::s.pairs,
        points = s.points + 1}

isHandEmpty : Player -> Bool
isHandEmpty p = 
  List.isEmpty p.hand 

isDeckEmpty : D.Deck -> Bool
isDeckEmpty d = 
  List.isEmpty d 

isGameOver : Game -> Bool 
isGameOver g = 
  isDeckEmpty g.deck || not (List.isEmpty (List.filter isHandEmpty g.players))

createDeck : Int -> D.Deck 
createDeck _ = 
-- for randomly shuffling a list, I utilized this reference: http://tylerscode.com/2016/06/list-shuffle-elm/
-- TODO: use time for random seed  
  let 
    randomList = Random.step (Random.list 52 (Random.int 1 100)) (Random.initialSeed 0) |> Tuple.first
  in 
    let 
      (rList, deck) = List.map2 (,) randomList D.startingDeck |> List.sortBy Tuple.first |> List.unzip 
    in
      deck 

findPlayer : List Player -> Int -> Player 
findPlayer players id =     
  case players of 
    p::ps ->  if p.id == id then 
                p 
              else 
                findPlayer ps id 
    _     ->  Debug.crash "findPlayer"

nextPlayer : Game -> Game 
nextPlayer g = 
  if g.current.id == List.length g.players then 
    { g | current = findPlayer g.players 1 }
  else 
    { g | current = findPlayer g.players (g.current.id + 1) }

drawCards : D.Deck -> Int -> (D.Deck, List D.Card)
drawCards d n = 
  let 
    taken = List.take n d 
  in 
    (List.drop n d, taken)

createPlayers : Int -> List Player -> D.Deck -> (List Player, D.Deck) 
createPlayers num players deck = 
  if num == 0 then 
    (players, deck) 
  else
    let (newDeck, startHand) = drawCards deck 5 in 
    let 
      newPlayer = { name = "Player" ++ toString num, id = num, hand = startHand, score = { pairs = [], points = 0} } 
    in 
      createPlayers (num - 1) (newPlayer::players) newDeck

createGame : Int -> Game 
createGame numPlayers = 
  let deck = createDeck 1 in 
    let 
      (players, newDeck) = createPlayers numPlayers [] deck  
      curr ps = 
        case ps of 
          p::rest -> p 
          [] -> Debug.crash "createGame: no players"
    in 
      { players = players, deck = newDeck, current = curr players }

startGame : Int -> Game 
startGame numPlayers = 
  let 
    game = createGame numPlayers 
  in
    runGame game

runGame : Game -> Game  
runGame g = 
  let 
    takeTurn g = 
      if g.current.id == 1 then 
        -- TODO: person playing --> wait for input
        g
      else 
        -- TODO: AI turn
        -- call remove when fishing, use drawCards to draw a card if Go Fish, update hands + score
        g 
  in 
    let 
      new = takeTurn g 
    in 
      if isGameOver new then 
        new 
      else 
        runGame (nextPlayer new)




     






