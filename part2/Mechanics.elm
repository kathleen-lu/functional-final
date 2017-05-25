module Mechanics exposing (..)

-- imports --
import Deck as D
import Random

-- type definitions --

type alias Hand = List D.Card 
type alias Score = { pairs: List D.Face, points: Int }
type alias Player = { name : String, id : Int, hand : Hand, score : Score }
type alias Game = { players : List Player, deck : D.Deck, current : Player, currFish : Maybe D.Face, asks : List (Player, D.Face), text : String, isGameOver : Bool }

-- Hand functions (checking for a card/removing, updating) -- 

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

updateHand : List Player -> Int -> Hand -> List Player 
updateHand players pID newHand = 
  (case players of 
    [] -> []
    p::ps ->  if p.id == pID then 
                ({p | hand = newHand})::ps
              else 
                p::(updateHand ps pID newHand))

-- Score functions -- 

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
                    Just (card, h) -> lp h (updateScore card.face score)
  in 
    let (newHand, newScore) = lp player.hand player.score in 
    { player | hand = newHand, score = newScore }

scorePlayers : List Player -> List Player
scorePlayers players = 
  case players of 
    [] -> []
    p::ps -> (scorePlayer p)::(scorePlayers ps)

scoreGame : Game -> Game 
scoreGame g = 
  let newPlayers = scorePlayers g.players in 
  {g | players = newPlayers, current = findPlayer newPlayers g.current.id}

findWinner : Game -> Player 
findWinner g =
  let 
    findMaxScore players maxs maxp = 
      (case players of 
        [] -> maxp 
        p::ps ->  if p.score.points > maxs then 
                    findMaxScore ps p.score.points p
                  else 
                    findMaxScore ps maxs maxp)
  in 
    findMaxScore g.players 0 g.current

-- Asks -- 

removeAsk : Player -> D.Face -> List (Player, D.Face) -> List (Player, D.Face)
removeAsk p f asks = 
  case asks of 
    [] -> []
    (player, face)::rest -> if player.id == p.id && face == f then 
                              rest 
                            else 
                              (player, face)::(removeAsk p f rest)

checkAsks : Player -> List (Player, D.Face) -> Maybe (Player, D.Face)
checkAsks p asks = 
  (case asks of 
    [] -> Nothing 
    (p2, f)::rest -> 
      if p2.id == p.id then 
        checkAsks p rest  
      else 
        (case remove f p.hand of 
          Nothing -> checkAsks p rest 
          Just _ -> Just (p2, f)))


playerNotInList : Int -> List Int -> List Int -> Maybe Int
playerNotInList currID pIDs players =
  (case pIDs of 
    [] -> Nothing 
    id::rest -> if List.member id players || currID == id then 
                  playerNotInList currID rest players 
                else  
                  Just id)

whoHasCard : D.Face -> List (Player, D.Face) -> List Int -> List Int
whoHasCard face asks players = 
  (case asks of
    [] -> players 
    (p, f)::rest -> if face == f then 
                      whoHasCard face rest (p.id::players)
                    else
                      whoHasCard face rest players)

asksList : List (Player, D.Face) -> List Int -> List Int
asksList asks numAsks = 
  let 
    addAsk i nAsks = 
      (case nAsks of 
        n::rest ->  if i == 1 then 
                     (n + 1)::rest 
                    else 
                      n::(addAsk (i - 1) rest)
        [] -> [])
  in 
    (case asks of 
      [] -> numAsks
      (p, _)::rest -> asksList rest (addAsk p.id numAsks))

mostCards : List Player -> List (Player, D.Face) -> Int -> Player 
mostCards players asks curr = 
  let 
    lengthHand players lenList = 
      (case players of 
        [] -> lenList 
        p::ps -> lengthHand ps ((List.length p.hand)::lenList))

    most nAsks nCards best bestpID i = 
      (case nAsks of 
        [] -> bestpID 
        n::rest -> 
          (case nCards of 
            [] -> bestpID 
            n2::rest2 ->  if (n2 - n) > best && i /= curr then 
                            most rest rest2 (n2 - n) i (i + 1)
                          else 
                            most rest rest2 best bestpID (i + 1))) 
  in 
  let numAsks = asksList asks [0, 0, 0, 0] in 
  let numCards = lengthHand players [] in 
    findPlayer players (most numAsks numCards 0 1 1)

-- Game Over -- 
isHandEmpty : Player -> Bool
isHandEmpty p = 
  List.isEmpty p.hand 

isDeckEmpty : D.Deck -> Bool
isDeckEmpty d = 
  List.isEmpty d 

isGameOver : Game -> Bool 
isGameOver g = 
  isDeckEmpty g.deck || not (List.isEmpty (List.filter isHandEmpty g.players))

-- update current player -- 

findPlayer : List Player -> Int -> Player 
findPlayer players id =     
  case players of 
    p::ps ->  if p.id == id then 
                p 
              else 
                findPlayer ps id 
    _     ->  Debug.crash "findPlayer"

nextPlayer : Game -> Player  
nextPlayer g = 
  if g.current.id == List.length g.players then 
    findPlayer g.players 1
  else 
    findPlayer g.players (g.current.id + 1)

-- Drawing cards -- 

drawCards : D.Deck -> Int -> (D.Deck, List D.Card)
drawCards d n = 
  let 
    taken = List.take n d 
  in 
    (List.drop n d, taken)

addCard : D.Card -> List Player -> Int -> (List Player, Bool) 
addCard c players pID = 
  (case players of 
    [] -> ([], False)
    p::ps ->  if p.id == pID then
                (case remove c.face p.hand of 
                  Nothing ->  (({p | hand = c::p.hand})::ps, False)
                  Just _ -> (({p | hand = c::p.hand})::ps, True))
              else 
                let (newPlayers, isScore) = addCard c ps pID in 
                  (p::newPlayers, isScore))

draw : Game -> Int -> (Game, Bool) 
draw g pID = 
  let (newDeck, topCard) = drawCards g.deck 1 in
    (case topCard of 
      [] -> Debug.crash "error: deck empty"
      c::cs -> 
        let (newPlayers, isScore) = addCard c g.players pID in
        ({ g | deck = newDeck, players = newPlayers}, isScore))

-- Fishing & Go Fish -- 

goFish : Game -> D.Face -> Game 
goFish g f = 
  let (drawCard, isScore) = draw g g.current.id in 
  let newGame = {drawCard | currFish = Nothing,
                            current = nextPlayer drawCard} in 
  let 
    ifScored chk g = 
      if chk then -- you formed a pair with the card you drew 
        scoreGame {g | asks = removeAsk drawCard.current f g.asks, 
                       text = g.text ++ "Go Fish. Player " ++ toString drawCard.current.id ++ " draws a card and scores!"}
      else  
        {g | text = g.text ++ "Go Fish. Player " ++ toString drawCard.current.id ++ " draws a card."}
  in 
  let scored = ifScored isScore newGame in 
    if isGameOver scored then 
      {scored | text = scored.text ++ "Game Over!", isGameOver = True}
    else 
      scored 

fish : Game -> Player -> Game 
fish g player = 
  (case g.currFish of 
    Nothing -> {g | text = "Please click on one of your cards first, then a player to ask for that card."}
    Just f -> let newG = {g | text = "Player " ++ toString g.current.id ++ " asked for a " ++ toString f ++ " from Player " ++ toString player.id ++ ". "} in
              let checkHand = remove f player.hand in
              (case checkHand of 
                Nothing -> -- Go Fish! 
                  let newGame = goFish newG f in 
                  {newGame | asks = (g.current, f)::newGame.asks}
                Just (c, h) -> 
                  (case remove f g.current.hand of 
                    Nothing -> Debug.crash "Error"
                    Just (c1, h1) -> 
                      let newGame = scoreGame {g | players = updateHand (updateHand g.players player.id h) g.current.id (c::c1::h1),
                                                   currFish = Nothing, asks = removeAsk player f (removeAsk g.current f g.asks),
                                                   text = newG.text ++ "Player " ++ toString g.current.id ++ " took a card from Player " ++ toString player.id ++ " and scored!"} in 
                        if isGameOver newGame then 
                          {newGame | text = newGame.text ++ " Game over!", isGameOver = True}
                        else 
                          {newGame | text = newGame.text ++ " It's still Player " ++ toString g.current.id ++ "'s turn."})))

makeMove : Game -> Game 
makeMove g =
  (case checkAsks g.current g.asks of 
    Nothing -> 
      (case g.current.hand of 
        [] -> Debug.crash "Error"
        c::cs -> 
          let newGame = {g | currFish = Just c.face} in 
          let bestPlayer = mostCards g.players g.asks g.current.id in 
            fish newGame bestPlayer) 
    Just (p2, f) -> 
      let newGame = {g | currFish = Just f} in 
        fish newGame p2)

decideMove : Game -> Game
decideMove g = 
  (case g.current.hand of 
    c::[] -> -- if one card in hand & losing, make a bad move on purpose
      let winner = findWinner g in 
      if winner.id /= g.current.id && winner.score.points > g.current.score.points then 
        let
          newGame = {g | currFish = Just c.face} 
          cardholders = whoHasCard c.face g.asks [] 
        in 
        let p = playerNotInList g.current.id [1, 2, 3, 4] cardholders in 
          (case p of 
            Nothing -> fish newGame (nextPlayer g)
            Just pID -> fish newGame (findPlayer g.players pID))
      else 
        makeMove g
    _ -> makeMove g)

smartAI : Game -> Game
smartAI g = 
  let afterMove = decideMove g in 
  if afterMove.isGameOver then 
    afterMove 
  else if afterMove.current.id == g.current.id then 
    let again = smartAI afterMove in  
      {again | text = afterMove.text ++ " " ++ again.text}
  else 
    afterMove 

-- Initial game setup -- 

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
    { players = players, deck = D.startingDeck, current = curr players, currFish = Nothing, asks = [], text = "Your turn. Choose a card to fish for by clicking on one of your cards.", isGameOver = False }




     






