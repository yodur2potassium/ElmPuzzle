import Browser
import Collage exposing (..)
import Collage.Layout exposing (stack)
import Collage.Render exposing (svg)
import Color exposing (Color)
import Html exposing (Html, button, div, h1, text)
import Html.Events exposing (onClick)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- Model

init : Tile
init =
    { sides = [ Empty, Connected False, Connected True, Empty ]
    , orientations = [ Up, Right, Down, Left ]
    }


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


update : Msg -> Tile -> Tile
update msg tile =
    case msg of
        Rotate ->
            rotateTile tile



-- View

testTile tile =
    stack
        [ line 50
            |> traced defaultLineStyle
        , square 50
            |> filled (uniform Color.lightYellow)
        ]
        |> svg

renderTile : Tile -> Html Msg
renderTile tile =
    stack
    [
        stack (stackPaths tile |> List.map (traced defaultLineStyle))
        , square 50
            |> filled (uniform Color.lightBlue)
    ]
        |> svg

drawSide : Side -> Orientation -> Maybe Path
drawSide side orientation =
    case hasConnection side of
        True ->
            Just (orientationToPath orientation)

        False ->
            Nothing

stackPaths : Tile -> List Path
stackPaths tile = List.map2 drawSide tile.sides tile.orientations |> List.filterMap identity
 

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
        [ button [ onClick Rotate ] [ text "Rotate" ]
        , div [] [ renderTile tile ]
        ]
