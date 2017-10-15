module DiamondMenu exposing (Config(..), Msg, State(..), open, subscriptions, update, view)

import Array exposing (Array)
import Char exposing (KeyCode)
import Dict exposing (Dict)
import Dom
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Json.Decode as Json
import Keyboard
import Task


-- MODEL


type State subject msg
    = State
        { open : Maybe subject
        , keyMap : Dict KeyCode Int
        , subjectActions : subject -> Array ( String, msg )
        }


type Config subject style variation msg
    = Config
        { attributes : List (Attribute variation msg)
        , openKeyCode : KeyCode
        , modalStyle : style
        , menuStyle : style
        , gridStyle : style
        , subjectStyle : style
        , actionStyle : style
        , actionWidth : Length
        , actionHeight : Length
        }


updateOpen : Maybe subject -> State subject msg -> State subject msg
updateOpen newOpen (State { open, keyMap, subjectActions }) =
    State { open = newOpen, keyMap = keyMap, subjectActions = subjectActions }



-- UPDATE


type Msg subject
    = OpenMenu subject
    | CloseMenu
    | PerformAction KeyCode
    | TriggerFocus Dom.Id
    | TriggerBlur Dom.Id
    | NoOp


update : Msg subject -> State subject msg -> ( State subject msg, Cmd (Msg subject), Maybe msg )
update msg ((State { open, keyMap, subjectActions }) as model) =
    case msg of
        OpenMenu subject ->
            ( updateOpen (Just subject) model, focus "diamondmenu", Nothing )

        CloseMenu ->
            ( updateOpen Nothing model, Cmd.none, Nothing )

        PerformAction keyCode ->
            case open of
                Just subject ->
                    ( model, Cmd.none, getAction (Dict.get keyCode keyMap) (subjectActions subject) )

                Nothing ->
                    ( model, Cmd.none, Nothing )

        TriggerFocus id ->
            ( model, focus id, Nothing )

        TriggerBlur id ->
            ( model, blur id, Nothing )

        NoOp ->
            ( model, Cmd.none, Nothing )


focus : String -> Cmd (Msg subject)
focus id =
    Dom.focus id
        |> Task.attempt (always NoOp)


blur : String -> Cmd (Msg subject)
blur id =
    Dom.blur id
        |> Task.attempt (always NoOp)


subscriptions : KeyCode -> State subject msg -> Sub (Msg subject)
subscriptions openKeyCode (State model) =
    Sub.batch
        [ Keyboard.ups
            (\keyCode ->
                if keyCode == openKeyCode then
                    CloseMenu
                else
                    NoOp
            )
        , Keyboard.downs
            (\keyCode ->
                case model.open of
                    Just _ ->
                        PerformAction keyCode

                    Nothing ->
                        NoOp
            )
        ]


getAction : Maybe Int -> Array ( String, msg ) -> Maybe msg
getAction mIndex subjectActions =
    case mIndex of
        Just index ->
            case Array.get index subjectActions of
                Just ( name, msg ) ->
                    Just msg

                Nothing ->
                    Nothing

        _ ->
            Nothing



-- VIEW


open : (Msg subject -> msg) -> subject -> List (Attribute variation msg)
open transform subject =
    [ attribute "tabindex" "10"
    , onWithOptions
        "mouseenter"
        { preventDefault = False, stopPropagation = True }
        (Json.at [ "target", "id" ] Json.string
            |> Json.map TriggerFocus
            |> Json.map transform
        )
    , onWithOptions
        "mouseleave"
        { preventDefault = False, stopPropagation = True }
        (Json.at [ "target", "id" ] Json.string
            |> Json.map TriggerBlur
            |> Json.map transform
        )
    , onWithOptions
        "keydown"
        { preventDefault = False, stopPropagation = False }
        (keyCode
            |> Json.map
                (\keyCode ->
                    if keyCode == 16 then
                        OpenMenu subject
                    else
                        NoOp
                )
            |> Json.map transform
        )
    ]


view : (Msg subject -> msg) -> State subject msg -> Config subject style variation msg -> Element style variation msg
view transform (State { open, subjectActions }) (Config { attributes, modalStyle, menuStyle, subjectStyle, actionStyle, gridStyle, actionWidth, actionHeight }) =
    case open of
        Just subject ->
            modal modalStyle attributes <|
                column menuStyle
                    [ id "diamondmenu", padding 10, spacing 20, attribute "tabindex" "-1" ]
                    [ h2 subjectStyle [ center ] (text (toString subject))
                    , grid
                        gridStyle
                        []
                        { columns = List.repeat 5 actionWidth
                        , rows = List.repeat 5 actionHeight
                        , cells = List.map (\i -> viewAction i actionStyle (Array.get i (subjectActions subject))) (List.range 0 8)
                        }
                    ]

        Nothing ->
            Element.empty


viewAction : Int -> style -> Maybe ( String, msg ) -> OnGrid (Element style variation msg)
viewAction index actionStyle mAction =
    cell
        { start = getActionCoordinates index
        , width = 1
        , height = 1
        , content =
            h2 actionStyle
                [ center ]
                (text
                    (case mAction of
                        Just ( name, _ ) ->
                            name

                        Nothing ->
                            "-"
                    )
                )
        }


getActionCoordinates : Int -> ( Int, Int )
getActionCoordinates index =
    case index of
        0 ->
            ( 2, 0 )

        1 ->
            ( 1, 1 )

        2 ->
            ( 3, 1 )

        3 ->
            ( 0, 2 )

        4 ->
            ( 2, 2 )

        5 ->
            ( 4, 2 )

        6 ->
            ( 1, 3 )

        7 ->
            ( 3, 3 )

        8 ->
            ( 2, 4 )

        _ ->
            ( 0, 0 )
