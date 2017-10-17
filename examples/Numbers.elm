module Main exposing (..)

import DiamondMenu as Dmd
import Element exposing (..)
import Element.Attributes exposing (..)
import EveryDict exposing (EveryDict)
import Html exposing (Html)
import KeyMap exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , update = update
        , view = view
        , subscriptions = \model -> Sub.map DmdMsg (Dmd.subscriptions model.diamondMenu Dmd.defaultConfig)
        }


initialModel : Model
initialModel =
    { number = 0
    , diamondMenu =
        Dmd.State
            { open = Nothing
            , keyMap = keyMap Qwerty
            , subjectActions =
                EveryDict.fromList
                    [ ( Number
                      , [ ( "zero (w)", Zero )
                        , ( "decrement (q)", Decrement )
                        , ( "increment (e)", Increment )
                        , ( "halve (a)", Halve )
                        , ( "round (s)", Round )
                        , ( "double (e)", Double )
                        ]
                      )
                    ]
            }
    }



-- MODEL


type alias Model =
    { number : Float
    , diamondMenu : Dmd.State Subject Msg
    }


type Subject
    = Number



-- UPDATE


type Msg
    = DmdMsg (Dmd.Msg Subject)
    | Increment
    | Decrement
    | Double
    | Halve
    | Zero
    | Round
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DmdMsg dmdMsg ->
            let
                ( newDmd, dmdCmd, mAction ) =
                    Dmd.update dmdMsg model.diamondMenu
            in
            case mAction of
                Just action ->
                    update action { model | diamondMenu = newDmd }

                Nothing ->
                    ( { model | diamondMenu = newDmd }, Cmd.map DmdMsg dmdCmd )

        Increment ->
            { model | number = model.number + 1 } ! [ Cmd.none ]

        Decrement ->
            { model | number = model.number - 1 } ! [ Cmd.none ]

        Double ->
            { model | number = model.number * 2 } ! [ Cmd.none ]

        Halve ->
            { model | number = model.number / 2 } ! [ Cmd.none ]

        Zero ->
            { model | number = 0 } ! [ Cmd.none ]

        Round ->
            { model | number = toFloat <| round model.number } ! [ Cmd.none ]

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    Element.viewport Dmd.defaultStyleSheet <|
        column Dmd.None
            [ height (percent 100), width (percent 100), padding 20 ]
            [ Dmd.withMenu DmdMsg Number (el Dmd.None [ width (px 50), height (px 50) ] <| bold (toString model.number))
            , Dmd.view DmdMsg model.diamondMenu Dmd.defaultConfig
            ]


stringWidth : String -> Attribute variation msg
stringWidth string =
    width (px (toFloat <| 40 + 10 * (String.length <| string)))
