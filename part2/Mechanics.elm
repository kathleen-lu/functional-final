module Mechanics exposing (..)

-- imports --
import Deck as D
import Random

-- type definitions --

type alias Hand = List D.Card 
type alias Score = { pairs: List D.Face, points: Int }
type alias Player = { name : String, id : Int, hand : Hand, score : Score }
type alias Game = { players : List Player, deck : D.Deck, current : Player, currFish : Maybe D.Face, asks : List (Player, D.Face) }

remove : D.Face -> Hand -> Maybe (D.Card, Hand) 
remove f h = 
-- if remove returns Nothing --> Go Fish!
  (case h of 
    c::cs ->  if f == c.face then 
                Just (c, cs) 
              else 
                (case remove f cs of 
                  Nothing -> Nothing 
                  Just (card, hand) -> Just (card, c::hand))
    [] -> Nothing)

updateScore : D.Face -> Score -> Score
updateScore f s = 
  { s | pairs = f::s.pairs,
        points = s.points + 1}

scorePlayer : Player -> Player 
scorePlayer player = 
  let 
    lp hand score = 
      case hand of 
        [] -> (hand, score)
        c::cs ->  case remove c.face cs of 
                    Nothing ->  let (newHand, newScore) = lp cs score in 
                                (c::newHand, newScore)
                    Just (card, h) -> lp h (updateScore c.face score)
  in 
    let (newHand, newScore) = lp player.hand player.score in 
    { player | hand = newHand, score = newScore }

scorePlayers : List Player -> List Player
scorePlayers players = 
  case players of 
    [] -> []
    p::ps -> (scorePlayer p)::(scorePlayers ps)

removeAsk : Player -> D.Face -> List (Player, D.Face) -> List (Player, D.Face)
removeAsk p f asks = 
  case asks of 
    [] -> []
    (player, face)::rest -> if player.id == p.id && face == f then 
                              rest 
                            else 
                              (player, face)::(removeAsk p f rest)

isHandEmpty : Player -> Bool
isHandEmpty p = 
  List.isEmpty p.hand 

isDeckEmpty : D.Deck -> Bool
isDeckEmpty d = 
  List.isEmpty d 

isGameOver : Game -> Bool 
isGameOver g = 
  isDeckEmpty g.deck || not (List.isEmpty (List.filter isHandEmpty g.players))

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

dealCards : List Player -> D.Deck -> (List Player, D.Deck)
dealCards players deck = 
  case players of 
    [] -> (players, deck)
    p::ps ->  let (newDeck, startHand) = drawCards deck 5 in 
              let (newPlayers, resDeck) = dealCards ps newDeck in 
              (({p | hand = startHand})::newPlayers, resDeck) 

createPlayers : Int -> List Player -> List Player  
createPlayers num players = 
  if num == 0 then 
    players  
  else
    let 
      newPlayer = { name = "Player" ++ toString num, id = num, hand = [], score = { pairs = [], points = 0} } 
    in 
      createPlayers (num - 1) (newPlayer::players)

createGame : Int -> Game 
createGame numPlayers = 
  let 
    players = createPlayers numPlayers []  
    curr ps = 
      case ps of 
        p::rest -> p 
        [] -> Debug.crash "createGame: no players"
  in 
    { players = players, deck = D.startingDeck, current = curr players, currFish = Nothing, asks = [] }

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




     






