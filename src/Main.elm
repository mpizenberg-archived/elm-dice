module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Random exposing (Generator)


main : Program () Model Msg
main =
    Browser.element
        { init = always ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model =
    { dice : List Die
    , visible : Bool
    }


type Die
    = One
    | Two
    | Three
    | Four
    | Five
    | Six


initialModel : Model
initialModel =
    { dice = []
    , visible = True
    }


type Msg
    = AddDie
    | RemoveDie
    | ToggleVisibility
    | Roll
    | NewDice (List Die)


dieGenerator : Generator Die
dieGenerator =
    Random.uniform One [ Two, Three, Four, Five, Six ]


diceGenerator : Int -> Generator (List Die)
diceGenerator nbDice =
    Random.list nbDice dieGenerator


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddDie ->
            ( { model | dice = model.dice ++ [ One ] }, Cmd.none )

        RemoveDie ->
            ( { model | dice = Maybe.withDefault [] (List.tail model.dice) }, Cmd.none )

        ToggleVisibility ->
            ( { model | visible = not model.visible }, Cmd.none )

        Roll ->
            ( model, Random.generate NewDice (diceGenerator <| List.length model.dice) )

        NewDice dice ->
            ( { model | dice = dice }, Cmd.none )


visibleDie : Die -> Html msg
visibleDie die =
    case die of
        One ->
            text "1,"

        Two ->
            text "2,"

        Three ->
            text "3,"

        Four ->
            text "4,"

        Five ->
            text "5,"

        Six ->
            text "6,"


hiddenDie : Die -> Html msg
hiddenDie =
    always (text "*,")


viewDie : Bool -> Die -> Html msg
viewDie visible =
    if visible then
        visibleDie

    else
        hiddenDie


visibilityToggleText : Bool -> String
visibilityToggleText visible =
    if visible then
        "hide"

    else
        "show"


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick AddDie ] [ text "+1" ]
        , button [ onClick RemoveDie ] [ text "-1" ]
        , button [ onClick Roll ] [ text "roll" ]
        , button [ onClick ToggleVisibility ] [ text (visibilityToggleText model.visible) ]
        , div [] (List.map (viewDie model.visible) model.dice)
        ]
