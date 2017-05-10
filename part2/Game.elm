module Game exposing(..)

--imports
import Deck as D
import Mechanics as M 
import Graphics as G
import Time
import Collage
import Element
import Html exposing (..)
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
-- { players : List Player, deck : D.Deck, current : Player }

type Msg = Start   

init : (Model, Cmd Msg)
init = (initialModel, Cmd.none)

initialModel : Model
initialModel = M.createGame 4 

--subscriptions : Model -> Sub Msg
--subscriptions model =

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  (model, Cmd.none)

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
