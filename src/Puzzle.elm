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
    , id = 1
    }

testBoard : Board
testBoard = 
    [ Tile [ Empty, Connected False, Connected True, Empty ] dir4 1
    , Tile [ Connected False, Connected False, Connected True, Empty ] dir4 2
    , Tile [ Empty, Connected False, Empty, Connected True ] dir4 3
    , Tile [ Empty, Empty, Connected True, Empty ] dir4 4
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
    , id : Int
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


cycleLeft : List a -> List a
cycleLeft list =
    case list of
        [] ->
            []

        x :: xs ->
            xs ++ [ x ]

rotateTile : Tile -> Int -> Tile
rotateTile tile id =
    { tile | sides = cycleLeft tile.sides, id = id }


-- Update

type Msg 
    = Rotate Int
    | Reset



update : Msg -> Tile -> Tile
update msg tile =
        -- let _ = Debug.log "Value of ID: " tile
        let _ = Debug.log "Tile: " tile
        in
        case msg of
            Rotate id ->
                rotateTile tile id 
            
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
        |> E.onClick (Rotate tile.id)

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
