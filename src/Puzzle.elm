import Browser
import Collage exposing (..)
import Collage.Layout exposing (stack, horizontal)
import Collage.Render exposing (svg)
import Collage.Events as E
import Color exposing (Color)
import Html exposing (Html, button, div, h1, text)
import Html.Events exposing (onClick)


main =
    Browser.sandbox { init = init, update = update, view = view }


-- Model

init : Tile
init =
    { sides = [ Empty, Connected False, Connected True, Empty ]
    , orientations = dir4
    }

testBoard : Board
testBoard = 
    [ Tile [ Empty, Connected False, Connected True, Empty ] dir4
    , Tile [ Connected False, Connected False, Connected True, Empty ] dir4
    , Tile [ Empty, Connected False, Empty, Connected True ] dir4
    , Tile [ Empty, Empty, Connected True, Empty ] dir4
    ]

dir4 = [ Up, Right, Down, Left ]

type Orientation
    = Up
    | Left
    | Down
    | Right


type Side
    = Connected Bool
    | Empty


type alias Tile =
    { sides : List Side
    , orientations : List Orientation
    }

type alias Board = List Tile

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
            xs ++ [ x ]

rotateTile : Tile -> Tile
rotateTile tile =
    { tile | sides = rotateLeft tile.sides }


-- Update

type Msg 
    = Rotate
    | Reset



update : Msg -> Tile -> Tile
update msg tile =
    case msg of
        Rotate ->
            rotateTile tile
        
        Reset ->
            init



-- View

renderBoard : Board -> Collage Msg
renderBoard board =
    horizontal <| List.map renderTile board

renderTile : Tile -> Collage Msg
renderTile tile =
    stack
    [
        stack (createPaths tile |> List.map (traced defaultLineStyle))
        , square 50
            |> filled (uniform Color.lightBlue)
    ]
        |> E.onClick Rotate

drawSide : Side -> Orientation -> Maybe Path
drawSide side orientation =
    case hasConnection side of
        True ->
            Just (orientationToPath orientation)

        False ->
            Nothing

createPaths : Tile -> List Path
createPaths tile = List.map2 drawSide tile.sides tile.orientations |> List.filterMap identity
 

orientationToPath : Orientation -> Path
orientationToPath orientation =
    case orientation of
        Up ->
            segment ( 0, 0 ) ( 0, 25 )

        Right ->
            segment ( 0, 0 ) ( 25, 0 )

        Down ->
            segment ( 0, 0 ) ( 0, -25 )

        Left ->
            segment ( 0, 0 ) ( -25, 0 )


view : Tile -> Html Msg
view tile =
    div []
        [ button [ onClick Reset ] [ text "Reset" ]
        , div [] [ renderBoard testBoard |> svg ]
        ]
