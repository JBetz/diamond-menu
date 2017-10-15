module Main exposing (..)

import Array exposing (Array)
import Color exposing (..)
import DiamondMenu as Dmd
import Element exposing (..)
import Element.Attributes exposing (..)
import Html exposing (Html)
import KeyMap exposing (..)
import Keyboard exposing (KeyCode)
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Style.Shadow as Shadow


main : Program Never Model Msg
main =
    Html.program
        { init = ( Model 0 (Dmd.State { open = Nothing, keyMap = keyMap Qwerty, subjectActions = subjectActions }), Cmd.none )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { number : Int
    , diamondMenu : Dmd.State Subject Msg
    }


type Subject
    = Number



-- UPDATE


type Msg
    = DmdMsg (Dmd.Msg Subject)
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DmdMsg dmdMsg ->
            let
                ( newDmd, dmdCmd, mActionIndex ) =
                    Dmd.update dmdMsg model.diamondMenu
            in
            ( { model | diamondMenu = newDmd }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    Element.viewport stylesheet <|
        column Root
            [ height (percent 100), width (percent 100) ]
            [ Dmd.view DmdMsg model.diamondMenu dmdConfig
            ]


dmdConfig : Dmd.Config Subject Style variation Msg
dmdConfig =
    Dmd.Config
        { attributes = [ center, paddingTop 200 ]
        , openKeyCode = 16
        , modalStyle = None
        , menuStyle = DiamondMenu
        , gridStyle = None
        , subjectStyle = DiamondMenuSubject
        , actionStyle = DiamondMenuAction
        , actionWidth = px 90
        , actionHeight = px 50
        }


subscriptions : KeyCode -> Model -> Sub Msg
subscriptions openKeyCode model =
    Sub.map DmdMsg (Dmd.subscriptions openKeyCode model.diamondMenu)


subjectActions : Subject -> Array ( String, Msg )
subjectActions subject =
    case subject of
        Number ->
            Array.fromList [ ( "run", NoOp ) ]



-- STYLES


type Style
    = None
    | Heading
    | Root
    | HR
    | DiamondMenu
    | DiamondMenuSubject
    | DiamondMenuAction


type Variation
    = Success
    | Failure
    | One
    | Two
    | WithMenu
    | WithoutMenu


stylesheet : StyleSheet Style Variation
stylesheet =
    Style.styleSheet
        [ style None []
        , style Heading
            [ Font.weight 700
            , variation One [ Font.size 21 ]
            , variation Two [ Font.size 18 ]
            , variation WithMenu [ focus [ Shadow.glow Color.blue 1 ] ]
            ]
        , style Root
            [ Color.background (Color.rgb 230 230 250) ]
        , style HR
            [ Border.top 2 ]
        , style DiamondMenu
            [ Color.background white
            , Shadow.simple
            , Border.rounded 20
            ]
        , style DiamondMenuSubject
            [ Font.weight 700
            , Font.size 18
            ]
        , style DiamondMenuAction
            [ Font.center ]
        ]
