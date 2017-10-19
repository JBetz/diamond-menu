module DiamondMenu exposing (Config, Msg, State(..), customConfig, defaultConfig, subscriptions, update, view, withMenu)

import Array exposing (Array)
import Char exposing (KeyCode)
import Dict exposing (Dict)
import Dom
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import EveryDict exposing (EveryDict)
import Json.Decode as Json
import Keyboard
import Keyboard.Extra exposing (Key(..), fromCode)
import Task


-- MODEL


type State subject msg
    = State
        { open : Maybe subject
        , keyMap : Dict KeyCode Int
        , subjectActions : EveryDict subject (List ( String, msg ))
        }


type Config subject style variation msg
    = Config
        { openKey : Key
        , attributes : List (Attribute variation msg)
        , actionWidth : Length
        , actionHeight : Length
        , styling : Styling style
        }


type alias Styling style =
    { modal : style
    , menu : style
    , grid : style
    , subject : style
    , action : style
    }


defaultConfig : Styling style -> Config subject style variation msg
defaultConfig styling =
    Config
        { openKey = Shift
        , attributes = [ center, paddingTop 100 ]
        , actionWidth = px 100
        , actionHeight = px 50
        , styling = styling
        }


customConfig :
    { openKey : Key
    , attributes : List (Attribute variation msg)
    , actionWidth : Length
    , actionHeight : Length
    , styling : Styling style
    }
    -> Config subject style variation msg
customConfig { openKey, attributes, actionWidth, actionHeight, styling } =
    Config
        { openKey = openKey
        , attributes = attributes
        , actionWidth = actionWidth
        , actionHeight = actionHeight
        , styling = styling
        }


updateOpen : Maybe subject -> State subject msg -> State subject msg
updateOpen newOpen (State { open, keyMap, subjectActions }) =
    State { open = newOpen, keyMap = keyMap, subjectActions = subjectActions }



-- UPDATE


type Msg subject
    = OpenMenu subject
    | CloseMenu
    | PerformAction Int
    | TriggerFocus Dom.Id
    | TriggerBlur Dom.Id
    | NoOp


update : Msg subject -> State subject msg -> ( State subject msg, Cmd (Msg subject), Maybe msg )
update msg ((State { open, keyMap, subjectActions }) as model) =
    case msg of
        OpenMenu subject ->
            ( updateOpen (Just subject) model, Cmd.none, Nothing )

        CloseMenu ->
            ( updateOpen Nothing model, Cmd.none, Nothing )

        PerformAction index ->
            case open of
                Just subject ->
                    ( model
                    , Cmd.none
                    , case getAction index subject subjectActions of
                        Just ( _, msg ) ->
                            Just msg

                        Nothing ->
                            Nothing
                    )

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


getAction : Int -> subject -> EveryDict subject (List ( String, msg )) -> Maybe ( String, msg )
getAction index subject subjectActions =
    case EveryDict.get subject subjectActions of
        Just actions ->
            Array.get index (Array.fromList actions)

        Nothing ->
            Nothing



-- VIEW


withMenu : Key -> (Msg subject -> msg) -> subject -> List (Attribute variation msg)
withMenu openKey transform subject =
    [ id (toString subject)
    , attribute "tabindex" "10"
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
        { preventDefault = True, stopPropagation = False }
        (keyCode
            |> Json.map
                (\keyCode ->
                    if fromCode keyCode == openKey then
                        OpenMenu subject
                    else
                        NoOp
                )
            |> Json.map transform
        )
    ]


view : (Msg subject -> msg) -> State subject msg -> Config subject style variation msg -> Element style variation msg
view transform (State { open, keyMap, subjectActions }) (Config { attributes, actionWidth, actionHeight, styling }) =
    case open of
        Just subject ->
            modal styling.modal attributes <|
                column styling.menu
                    [ id "diamondmenu", padding 10, spacing 20, attribute "tabindex" "-1" ]
                    [ h2 styling.subject [ center ] (text (toString subject))
                    , grid
                        styling.grid
                        []
                        { columns = List.repeat 5 actionWidth
                        , rows = List.repeat 5 actionHeight
                        , cells = List.map (\i -> viewAction i styling.action (getAction i subject subjectActions)) (List.range 0 8)
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
            el actionStyle
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



-- SUBSCRIPTIONS


subscriptions : State subject msg -> Config subject style variation msg -> Sub (Msg subject)
subscriptions (State { open, keyMap }) (Config { openKey }) =
    Sub.batch
        [ Keyboard.ups
            (\keyCode ->
                if fromCode keyCode == openKey then
                    CloseMenu
                else
                    NoOp
            )
        , Keyboard.downs
            (\keyCode ->
                case open of
                    Just _ ->
                        case Dict.get keyCode keyMap of
                            Just index ->
                                PerformAction index

                            Nothing ->
                                NoOp

                    Nothing ->
                        NoOp
            )
        ]
