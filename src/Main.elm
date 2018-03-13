module Main exposing (..)

import Html exposing (Html, text, div, h1, img, p, button)
import Html.Attributes exposing (value, value)
import Html.Events exposing (onClick, onInput)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Random exposing (..)
import Random.Array exposing (..)
import Array exposing (Array)


---- MODEL ----


type alias Model =
    { endEarlyArray : (Array Bool)
    , depth : Int }


init : ( Model, Cmd Msg )
init =
    ( {
      endEarlyArray = (Array.fromList [True, False])
      , depth = 10
      }
      , Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | GenerateRandomList
    | NewRandomArray (Array Bool)
    | Depth String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      ( model, Cmd.none )
    GenerateRandomList ->
      ( model, Random.generate NewRandomArray (endEarlyListGenerator (8)) )
    NewRandomArray array ->
      ( { model | endEarlyArray = array }, Cmd.none )
    Depth newDepth ->
      ( { model | depth = (Result.withDefault 8 (String.toInt newDepth)) }, Cmd.none )

endEarlyListGenerator : Int -> Generator (Array Bool)
endEarlyListGenerator length =
  Random.Array.array length (
    Random.map
      (\randValue ->
        if randValue >= 0.85 then
          True
        else
          False
      )
      (Random.float 0 1))


---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ svg
          [ width "500", height "500", viewBox "0 0 500 500" ]
          (mondrian model.depth model.endEarlyArray 500 500 0 0)
        , p [] [ Html.text (toString model.endEarlyArray) ]
        , Html.input [ type_ "number", value (toString model.depth), onInput Depth ] []
        , button [ onClick GenerateRandomList ] [ Html.text "Randomize" ]
        ]

mondrian : Int -> Array Bool -> Int -> Int -> Int -> Int -> List (Svg msg)
mondrian depth endEarlyArray currentWidth currentHeight currentX currentY =
  let
    even = (depth % 2 == 0)
    endEarlyLeft = Maybe.withDefault False (Array.get depth endEarlyArray)
    endEarlyRight = Maybe.withDefault False (Array.get (depth + 1) endEarlyArray)
    randomDepthLeft = if endEarlyLeft then 0 else depth - 1
    randomDepthRight = if endEarlyRight then 0 else depth - 1
  in
    case even of
      True ->
        let
          halfWidth = round (toFloat currentWidth / 2)
          rightX = halfWidth + currentX
          halfHeight = round (toFloat currentHeight / 2)
          lowerY = halfHeight + currentY
        in
          case depth of
            0 ->
              [ rect [ x (toString currentX), y (toString currentY), width (toString halfWidth), height (toString currentHeight), fill "red", stroke "black" ] []
              , rect [ x (toString rightX), y (toString currentY), width (toString halfWidth), height (toString currentHeight), fill "blue", stroke "black" ] []]
            2 ->
              [ rect [ x (toString currentX), y (toString currentY), width (toString currentHeight), height (toString halfHeight), fill "blue", stroke "black" ] []
              , rect [ x (toString currentX), y (toString lowerY), width (toString currentHeight), height (toString halfHeight), fill "red", stroke "black" ] []]
            _ ->
              List.concat [(mondrian randomDepthLeft endEarlyArray halfWidth currentHeight currentX currentY)
              , (mondrian randomDepthRight endEarlyArray halfWidth currentHeight rightX currentY)]

      -- no branch for depth 0 here required as 0 always even
      False ->
        let
          halfHeight = round (toFloat currentHeight / 2)
          lowerY = halfHeight + currentY
        in
            List.concat [(mondrian randomDepthLeft endEarlyArray currentWidth halfHeight currentX currentY)
            , (mondrian randomDepthRight endEarlyArray currentWidth halfHeight currentX lowerY)]


---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
