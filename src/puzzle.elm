import Browser

import Html exposing (Html, button, div)

-- Model

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