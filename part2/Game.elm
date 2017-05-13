module Game exposing(..)

--imports
import Deck as D
import Mechanics as M 
import Graphics as G
import Random
import Collage
import Element
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
  let graphic =  Element.toHtml <| Collage.collage 1000 600 [G.renderHand testHand, G.renderFaceDown] in 
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

