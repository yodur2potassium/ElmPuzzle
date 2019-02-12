import Browser

import Html exposing (Html, button, div, h1, text)
import Html.Events exposing (onClick)

import Collage exposing (..)
import Collage.Render exposing (svg)
import Color exposing (Color)

main =
  Browser.sandbox { init = init, update = update, view = view }

-- Model

type alias Model = {}

init: Tile
init = {
    sides = [ Connected True, Empty, Connected False, Empty ],
    orientation = Up
    }

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

rotateLeft : List a -> List a
rotateLeft list = 
    case list of
        [] ->
            []
        
        x :: xs ->
            xs ++ [x]

-- Update

type Msg = Rotate
update : Msg -> Tile -> Tile
update msg tile =
    case msg of
        Rotate ->
            tile

-- View

testTile = square 50
            |> filled (uniform Color.red)
            |> svg

view : Tile -> Html Msg
view tile =
    div [] 
        [ button [ onClick Rotate ] [ text "Rotate" ]
        , div [] [ testTile ] ]
