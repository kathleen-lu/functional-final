module Game exposing(..)

--imports
import Deck as D
import Mechanics as M 
import Graphics as G exposing (..)
import Random
import Collage
import Element
import Text
import String exposing(..)
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes as Attribute exposing(style)

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
-- { players : List Player, deck : D.Deck, current : Player, currFish = Face, asks = List (Player, Face) }

type Msg = NoOp | StartGame (List Int) | Score | Choose D.Card | Fish M.Player | GoFish

init : (Model, Cmd Msg)
init = (initialModel, randomList StartGame)

initialModel : Model
initialModel = M.createGame 4 

--subscriptions : Model -> Sub Msg
--subscriptions model =

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of 
    NoOp -> (model, Cmd.none)
    StartGame ranList -> 
      let newDeck = shuffleDeck D.startingDeck ranList in
      let 
        (newPlayers, resDeck) = M.dealCards model.players newDeck 
      in 
        ({model | players = newPlayers, deck = resDeck}, Cmd.none)
    Score ->  -- TODO: figure out how to show this
      ({model | players = M.scorePlayers model.players}, Cmd.none) 
    Choose card ->
      ({model | currFish = Just card.face}, Cmd.none)
    Fish player -> 
      case model.currFish of 
        Nothing -> Debug.crash "error: didn't select card to fish yet"
        Just f -> let checkHand = M.remove f player.hand in
                  case checkHand of 
                    Nothing -> ({model | currFish = Nothing, asks = (model.current, f)::model.asks}, Cmd.none)
                    -- TODO: remove currFish from hand, replace other player's hand, update score, update current and start AI
                    Just (c, h) -> ({model | currFish = Nothing, asks = M.removeAsk player f model.asks}, Cmd.none)
    GoFish -> (model, Cmd.none) -- TODO

view : Model -> Html Msg
view model =
  let title = text "Go Fish" in
  let display = text (" " ++ toString model) in
  let testHand = [{face=D.Ace, suit=D.Heart}, {face = D.Four, suit = D.Spade}, {face = D.Seven, suit = D.Clover}, {face = D.Six, suit = D.Diamond}] in
  let graphic =  Element.toHtml <| Collage.collage 1500 1500 [renderGame model] in 
    div [mainStyle] [h1 [titleStyle] [title], div [] [graphic, display]]

--- attribute styles
mainStyle : Attribute msg
mainStyle = style [("width", "750px"), ("margin", "5%"), ("font-family", "sans-serif"), ("text-align", "center")]

titleStyle : Attribute msg
titleStyle = style [("text-align", "center")]

--- Update functions ----  
randomList : (List Int -> Msg) -> Cmd Msg 
randomList msg = 
  -- for randomly shuffling a list, I utilized this reference: http://tylerscode.com/2016/06/list-shuffle-elm/
  Random.int 1 100 |> Random.list 52 |> Random.generate msg  

shuffleDeck : D.Deck -> List Int -> D.Deck 
shuffleDeck deck rList = 
  List.map2 (,) rList deck |> List.sortBy Tuple.first |> List.unzip |> Tuple.second 


--- graphics functions that i moved because of import things
-- anton this is what you should call in order to generate the whole game cool
renderGame : Model -> Collage.Form
renderGame model = 
  let ps = renderPlayerHands model.players 0 in
  let text = renderMoveText model in
    Collage.group [ps,text]

renderMoveText : Model -> Collage.Form
renderMoveText model =
  let move = "this is where the next move goes" in 
    Collage.move (G.h,G.w) <|
      Collage.toForm <| Element.justified <| Text.height 30 <| Text.fromString move

--renderScore : Model -> Collage.Form
--renderScore model = 
--  let score = "score: " ++ toString model.score.points in 
--    Collage.move (G.h,G.w) <|
--      Collage.toForm <| Element.justified <| Text.height 30 <| Text.fromString score

renderPlayerHands : List M.Player -> Float -> Collage.Form
renderPlayerHands players rotate =
  case players of
    [] -> G.emptyCollage
    fst::rst ->
      let applyMove = 
          if rotate == 0 then
            Collage.move (-G.w/2.5,-G.h/2.5) 
          else if rotate == 1 then
            Collage.move (-G.w*2,-G.h/2.5)
          else if rotate == 2 then
            Collage.move (G.w*4,G.h*3) 
          else 
            Collage.move (G.w*5.5,G.h*2.5) 
      in
      if fst.name == "Player1" then
        Collage.group [ applyMove <| renderPlayerNameHand fst rotate True, 
                        renderPlayerHands rst (rotate+1)
                      ]
      else 
        Collage.group [ applyMove <| renderPlayerNameHand fst rotate False,
                        renderPlayerHands rst (rotate+1)
                      ]

renderPlayerNameHand : M.Player -> Float -> Bool -> Collage.Form
renderPlayerNameHand player rotate isFaceUp =
  let applyMove = 
    if (round rotate) % 2 == 0 then
      Collage.move (G.w*2,-G.h)
    else 
      Collage.move (G.w*2, G.h)
    in 
  let pname = applyMove <| renderPlayerName player in 
  let handInner =
    if isFaceUp then G.renderHand player.hand
    else G.renderFacedownHand player.hand
    in 
    Collage.rotate (degrees (rotate*90)) <| Collage.group [handInner, pname]


renderPlayerName : M.Player -> Collage.Form
renderPlayerName player =
  let cosmetics = (dropRight 1 player.name) ++ " " ++ (right 1 player.name) in 
    Collage.toForm <| Element.justified <| Text.height 25 <| Text.fromString cosmetics 
