import Browser

import Html exposing (Html, button, div, h1, text)
import Html.Events exposing (onClick)

main =
  Browser.sandbox { init = init, update = update, view = view }

-- Model

type alias Model = {}

init: Model
init = {}

type Orientation = Up | Left | Down | Right

type Side
    = Connected Bool
    | Empty

type alias Tile = { sides : List Side, orientation : Orientation }

hasConnection : Side -> Bool
hasConnection side = 
    case side of
        Connected _ ->
            True
    
        Empty ->
            False

isConnected : Side -> Bool
isConnected side =
    case side of
        Connected True ->
            True 
        
        Connected False -> 
            False

        Empty ->
            False

-- Update

type Msg = Rotate
update : Msg -> Model -> Model
update msg model =
    case msg of
        Rotate ->
            model

-- View

view : Model -> Html Msg
view model =
    div [] [ button [ onClick Rotate ] [ text "Rotate" ] ]