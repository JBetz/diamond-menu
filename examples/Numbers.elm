module Main exposing (..)

import Color exposing (..)
import DiamondMenu as Dmd
import Element exposing (..)
import Element.Attributes exposing (..)
import EveryDict exposing (EveryDict)
import Html exposing (Html)
import KeyMap exposing (..)
import Keyboard.Extra exposing (Key(Shift))
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Style.Shadow as Shadow


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , update = update
        , view = view
        , subscriptions = \model -> Sub.map DmdMsg (Dmd.subscriptions model.diamondMenu dmdConfig)
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


dmdConfig : Dmd.Config subject Style variation msg
dmdConfig =
    Dmd.defaultConfig
        { modal = None
        , menu = DiamondMenu
        , grid = None
        , subject = DiamondMenuSubject
        , action = DiamondMenuAction
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
view { number, diamondMenu } =
    Element.viewport stylesheet <|
        column Root
            [ height (percent 100), width (percent 100), padding 20 ]
            [ el Object ([ stringWidth <| toString number, height (px 50), padding 20, vary WithMenu True ] ++ Dmd.withMenu Shift DmdMsg Number) <|
                bold (toString number)
            , Dmd.view DmdMsg diamondMenu dmdConfig
            ]


stringWidth : String -> Attribute variation msg
stringWidth string =
    width (px (toFloat <| 40 + 10 * (String.length <| string)))



-- STYLING


type Style
    = None
    | Root
    | Object
    | DiamondMenu
    | DiamondMenuSubject
    | DiamondMenuAction


type Variation
    = WithMenu


stylesheet : StyleSheet Style Variation
stylesheet =
    Style.styleSheet
        [ style None []
        , style Root
            [ Color.background (Color.rgb 230 230 250) ]
        , style Object
            [ Font.center
            , variation WithMenu [ focus [ Shadow.glow Color.blue 1 ] ]
            ]
        , style DiamondMenu
            [ Color.background white
            , Shadow.simple
            , Border.rounded 20
            ]
        , style DiamondMenuSubject
            [ Font.center
            , Font.weight 700
            , Font.size 18
            , variation WithMenu [ Style.focus [ Shadow.glow Color.blue 1 ] ]
            ]
        , style DiamondMenu
            [ Color.background white
            , Shadow.simple
            , Border.rounded 20
            ]
        , style DiamondMenuAction
            [ Font.center ]
        ]
