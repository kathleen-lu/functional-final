module Graphics exposing(..)

--imports
import Deck as D 
import Mechanics as M 
import Time
import Html exposing (Html)
import Html.Attributes as Attr

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
  let display = Html.text (" " ++ toString model) in
  Html.div [] [display]


