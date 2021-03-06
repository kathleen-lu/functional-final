module Game exposing(..)

--imports
import Deck as D
import Mechanics as M 
import Graphics as G exposing (..)
import Random
import Collage
import Element
import Text
import Image
import String exposing(..)
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing(..)

----------------------------------------------------------------------

main : Program Never Model Msg
main = 
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }

type alias Model = M.Game 
-- { players : List Player, deck : D.Deck, current : Player, currFish : Face, asks : List (Player, Face), text : String }

type Msg = NoOp | StartGame (List Int) | Choose D.Card | Fish M.Player | NextTurn | Restart

init : (Model, Cmd Msg)
init = (initialModel, randomList StartGame)

initialModel : Model
initialModel = M.createGame 4 

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of 
    NoOp -> (model, Cmd.none)
    StartGame ranList -> 
      let newDeck = shuffleDeck D.startingDeck ranList in
      let (newPlayers, resDeck) = M.dealCards model.players newDeck in
      let newGame = {model | players = newPlayers, deck = resDeck} in
        (M.scoreGame newGame, Cmd.none)
    Choose card ->
      if model.current.id == 1 then
        ({model | currFish = Just card.face, text = "Your turn. Now click a player to ask for that card, or change your selection by clicking on another card."}, Cmd.none)
      else 
        ({model | text = "It's not your turn! Click the Next Turn button for the AI to move."}, Cmd.none)
    Fish player -> 
      if model.current.id == 1 then 
        if player.id == model.current.id then
          ({model | text = "You can't ask for a card from yourself!"}, Cmd.none)
        else 
          let newGame = M.fish model player in
            (newGame, Cmd.none)
      else 
        ({model | text = "It's not your turn! Click the Next Turn button for the AI to move."}, Cmd.none)
    NextTurn -> 
      if model.current.id == 1 then 
        ({model | text = "It's your turn. Please click on one of your cards. Click the Next Turn button only on AI turns."}, Cmd.none)
      else 
        let newGame = M.smartAI model in 
          (newGame, Cmd.none)
    Restart -> init 


view : Model -> Html Msg
view model =
  let title = text "Go Fish!" in
  let display = text (" " ++ toString model) in
  if model.isGameOver then 
    let winner = M.findWinner model in
    let wtext = text ("Game over! " ++ winner.name ++ " has won with a score of " ++ (toString winner.score.points) ++ "! Play again?") in 
      div [id "container", containerStyle] 
        [div [mainStyle model] [ h1 [titleStyle] [title], 
         div [ style [("margin", "auto"), ("display", "block")], moveTextStyle]
              [renderButtonHtml Restart "Restart", wtext] ]]
  else 
    let players = renderFacedownHandHtml model in
    let btn = renderButtonHtml NextTurn "Next Turn" in 
    let moveText = renderMoveTextHtml model in
      div [id "container", containerStyle] 
                       [div [mainStyle model] [ h1 [titleStyle] [title], 
                        div [ class "fish"] [],
                        div [style [("display", "block")]] players,
                        div [style [("clear", "both"), ("padding-bottom", "6%")]] [btn, moveText]]]

--- attribute styles
mainStyle : Model -> Attribute msg
mainStyle model =
    style [ calculateWidth model,
            ("margin", "auto"), ("display", "block"),
            ("font-family", "sans-serif"), ("text-align", "center")]

containerStyle : Attribute msg
containerStyle = style [ ("background-image", "url(fish_small_light.png)") ]

titleStyle : Attribute msg
titleStyle = style [ ("text-align", "center"), ("font-size", "40px") ]

buttonStyle : Attribute msg
buttonStyle = style [ ("font-size", "28px"), ("padding", "1%"), ("border", "none"), 
                      ("margin", "3%"), ("background-color", "#dddddd")]

moveTextStyle : Attribute msg
moveTextStyle = style [ ("font-size", "30px") ]

calculateWidth : Model -> (String, String)
calculateWidth model =
  let player1 = List.length (noMaybes <| List.head model.players).hand in
  let player3 = List.length (getPlayer3 model.players).hand in 
  let width1 = (detWidth player1)*player1 in
  let width3 = (detWidth player3)*player3 in
    ("width", (toString <| Basics.max width1 width3) ++ "px")

detWidth : Int -> Int 
detWidth length = 
  if length <= 1 then 270
  else if length == 2 then 250
  else if length == 3 then 230
  else if length == 4 then 210
  else 190

--- Update functions ----  
randomList : (List Int -> Msg) -> Cmd Msg 
randomList msg = 
  -- for randomly shuffling a list, I utilized this reference: http://tylerscode.com/2016/06/list-shuffle-elm/
  Random.int 1 100 |> Random.list 52 |> Random.generate msg  

shuffleDeck : D.Deck -> List Int -> D.Deck 
shuffleDeck deck rList = 
  List.map2 (,) rList deck |> List.sortBy Tuple.first |> List.unzip |> Tuple.second 

--- player styles
faceUpCardStyle : Attribute msg 
faceUpCardStyle = style [("float", "left"), ("margin", "2px")]

faceDownCardStyle : Attribute msg 
faceDownCardStyle = style [("margin", "2px")]

player1Style : Attribute msg
player1Style = style [ ("clear", "both"), ("font-size", "20px"), ("text-align", "center") ]

currentPlayerStyle : Attribute msg 
currentPlayerStyle = style [ ("background-color", "#00FFFF"), ("padding", "1%"),
                             ("clear", "both"), ("font-size", "20px"), ("text-align", "center")
                             ]

faceDownHandStyle : M.Player -> Attribute msg
faceDownHandStyle player =
  if player.name == "Player2" then
    style [("float", "left"), ("margin-top", "5%"), ("margin-right", "5%"), calcExtraPadding player]
  else if player.name == "Player3" then
    style [("float", "left"), ("margin-bottom", "30%"), ("display", "block")]
  else
    style [("float", "right"), ("margin-top", "5%"), calcExtraPadding player]

faceUpHandStyle :  M.Player -> Attribute msg
faceUpHandStyle player = 
  style [("float", "left"), ("margin", "auto"), ("display", "block")]

calcExtraPadding : M.Player -> (String, String)
calcExtraPadding player =
  let length = List.length player.hand in 
  let val = G.w*(5-length) in
    if length < 4 then ("padding-bottom", (toString val) ++ "px")
    else ("padding", "auto")

--- rendering functions
renderMoveTextHtml : Model -> Html Msg
renderMoveTextHtml model =
  let move = model.text in 
    div [class "move-text", moveTextStyle] [text move]

renderButtonHtml : Msg -> String -> Html Msg
renderButtonHtml msg str = 
  button [buttonStyle, onClick msg] [text str]

renderHandHtml : M.Player -> Maybe D.Face -> Html Msg
renderHandHtml player1 selected =
  let htmlcards = List.map (\x -> 
    case selected of 
      Nothing -> renderFaceUpHtml x
      Just s -> if s == x.face then renderFaceUpSelectHtml x else renderFaceUpHtml x) player1.hand in 
    div [ class player1.name, faceUpHandStyle player1] 
      (htmlcards ++ [div [player1Style] [ text <| nameAndScore player1]])

renderFaceUpHtml : D.Card -> Html Msg
renderFaceUpHtml card = 
  div [ class (cardClass card), faceUpCardStyle, onClick (Choose card)] 
    [Element.toHtml <| Collage.collage G.w G.h <| [renderFaceUp card]]

renderFaceUpSelectHtml : D.Card -> Html Msg
renderFaceUpSelectHtml card = 
  div [ class (cardClass card), faceUpCardStyle, onClick (Choose card)] 
    [Element.toHtml <| Collage.collage G.w G.h <| [renderFaceUpSelect card]]

renderFaceDownHtml : D.Card -> Bool -> Html Msg
renderFaceDownHtml card isSideways =
  if isSideways then 
    div [faceDownCardStyle] 
      [Element.toHtml <| Collage.collage G.h G.w <| [Collage.rotate (degrees 90) <| renderFaceDown]]
  else
    div [faceUpCardStyle] 
      [Element.toHtml <| Collage.collage G.w G.h <| [renderFaceDown]]

renderFacedownHandHtml : Model -> List(Html Msg)
renderFacedownHandHtml model = 
  rfdhHelper (reorderPlayers model.players) model.currFish

reorderPlayers : List M.Player -> List M.Player
reorderPlayers players = 
  (List.drop 1 players) ++ (List.take 1 players)

rfdhHelper : List M.Player -> Maybe D.Face -> List(Html Msg)
rfdhHelper players currFish = 
  List.map 
    (\player -> 
      if player.name /= "Player1" then
        let length = List.length player.hand in 
        let isSideways = if player.name == "Player2" || player.name == "Player4" then True else False in
        let playerName = [div [player1Style][text <| nameAndScore player]] in
          div [ class player.name, (faceDownHandStyle player), onClick (Fish player)] 
            ((List.map (\x -> renderFaceDownHtml x isSideways) player.hand) ++ playerName)
      else renderHandHtml player currFish) 
    players

--- misc helper functions
nameAndScore : M.Player -> String
nameAndScore player = 
  (prettyName player) ++ " " ++ (scoreString player)

prettyName : M.Player -> String
prettyName player = 
  (dropRight 1 player.name) ++ " " ++ (right 1 player.name)

scoreString : M.Player -> String
scoreString player = 
  "Score: " ++ (toString player.score.points)

noMaybes : Maybe M.Player -> M.Player
noMaybes player = 
  case player of
    Nothing -> Debug.crash "should not take this case in noMaybes"
    Just a -> a

getPlayer3 : List M.Player -> M.Player
getPlayer3 players = 
  case players of
    [] -> Debug.crash "should not ever happen"
    p::rest -> if p.name == "Player3" then p else getPlayer3 rest